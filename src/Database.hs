{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database where

import           Data.Text                    (Text)
import qualified Data.Text                    as T

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Migrate
import           Database.Beam.Sqlite.Syntax
import           Database.SQLite.Simple       (Connection)

import           Control.Monad                (void)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime

import           Lens.Micro                   ((^.))

data NotificationPercent
  = Zero
  | TwentyFive
  | Fifty
  | SeventyFive
  | Hundred
  deriving (Eq, Ord, Show, Read)

instance BeamMigrateSqlBackend be =>
         HasDefaultSqlDataType be NotificationPercent where
  defaultSqlDataType = defaultSqlDataType . fmap (T.pack . show)

instance HasSqlValueSyntax be String =>
         HasSqlValueSyntax be NotificationPercent where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite NotificationPercent where
  fromBackendRow = read . T.unpack <$> fromBackendRow

notificationPercentDataType :: DataType Sqlite NotificationPercent
notificationPercentDataType = DataType sqliteTextType

data ProposalT f =
  Proposal
    { _proposalAccount             :: Columnar f Text
    , _proposalAccountOwner        :: Columnar f Text
    , _proposalServiceUnits        :: Columnar f Int
    , _proposalEndDate             :: Columnar f LocalTime
    , _proposalNotificationPercent :: Columnar f NotificationPercent
    , _proposalLocked              :: Columnar f Bool
    , _proposalsSubmitted          :: Columnar f Int
    , _proposalServiceUnitsUnused  :: Columnar f Int
    }
  deriving (Generic)

type Proposal = ProposalT Identity

Proposal (LensFor proposalAccount) (LensFor proposalAccountOwner) (LensFor proposalServiceUnits) (LensFor proposalEndDate) (LensFor proposalNotificationPercent) (LensFor proposalLocked) (LensFor proposalsSubmitted) (LensFor proposalServiceUnitsUnused) =
  tableLenses

deriving instance Show Proposal

deriving instance Eq Proposal

type ProposalId = PrimaryKey ProposalT Identity

instance Beamable ProposalT

instance Table ProposalT where
  data PrimaryKey ProposalT f = ProposalId (Columnar f Text)
                                deriving (Generic, Beamable)
  primaryKey = ProposalId . _proposalAccount

newtype ProposalDb f =
  ProposalDb
    { _proposals :: f (TableEntity ProposalT)
    }
  deriving (Generic, Database be)

ProposalDb (TableLens proposals) = dbLenses

proposalDb :: DatabaseSettings Sqlite ProposalDb
proposalDb = unCheckDatabase $ evaluateDatabase initialSetupStep

initialSetup :: Migration Sqlite (CheckedDatabaseSettings Sqlite ProposalDb)
initialSetup =
  ProposalDb <$>
  (createTable "proposals" $
   Proposal
     { _proposalAccount = field "account" (varchar Nothing) notNull unique
     , _proposalAccountOwner = field "accountOwner" (varchar Nothing) notNull
     , _proposalServiceUnits = field "serviceUnits" int notNull
     , _proposalEndDate = field "endDate" timestamptz notNull
     , _proposalNotificationPercent =
         field "notificationPercent" notificationPercentDataType notNull
     , _proposalLocked = field "locked" boolean notNull
     , _proposalsSubmitted = field "proposalsSubmitted" int notNull
     , _proposalServiceUnitsUnused = field "serviceUnitsUnused" int notNull
     })

initialSetupStep ::
     MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite ProposalDb)
initialSetupStep = migrationStep "initial_setup" (const initialSetup)

allowDestructive :: Monad m => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks {runIrreversibleHook = pure True}

migrateDb ::
     Connection -> IO (Maybe (CheckedDatabaseSettings Sqlite ProposalDb))
migrateDb conn =
  runBeamSqliteDebug putStrLn conn $
  bringUpToDateWithHooks allowDestructive migrationBackend initialSetupStep

newtype Account =
  Account
    { account :: Text
    }

newtype AccountOwner =
  AccountOwner
    { accountOwner :: Text
    }

newtype ServiceUnits =
  ServiceUnits
    { serviceUnits :: Int
    }

createProposal :: Connection -> Account -> AccountOwner -> ServiceUnits -> IO ()
createProposal conn (Account account) (AccountOwner owner) (ServiceUnits units) = do
  currTime <- getCurrentTime
  currTimeZone <- getCurrentTimeZone
  let endDate = addGregorianYearsClip 1 (utctDay currTime)
  runBeamSqliteDebug putStrLn conn $
    runInsert $
    insert (_proposals proposalDb) $
    insertExpressions
      [ Proposal
          (val_ account)
          (val_ owner)
          (val_ units)
          (val_ $ utcToLocalTime currTimeZone currTime {utctDay = endDate})
          (val_ Zero)
          (val_ False)
          (val_ 0)
          (val_ 0)
      ]

readProposal :: Connection -> Account -> IO (Maybe (ProposalT Identity))
readProposal conn (Account account) =
  runBeamSqliteDebug putStrLn conn $
  runSelectReturningOne $ lookup_ (proposalDb ^. proposals) (ProposalId account)

addServiceUnits :: Connection -> Account -> ServiceUnits -> IO ()
addServiceUnits conn account@(Account name) (ServiceUnits value) = do
  exists <- readProposal conn account
  case exists of
    Just proposal -> do
      let updatedProposal =
            proposal
              {_proposalServiceUnits = _proposalServiceUnits proposal + value}
      runBeamSqliteDebug putStrLn conn $
        runUpdate $ save (proposalDb ^. proposals) updatedProposal
    Nothing -> print $ "Error: Account `" ++ T.unpack name ++ "` Doesn't Exist"

changeServiceUnits :: Connection -> Account -> ServiceUnits -> IO ()
changeServiceUnits conn account@(Account name) (ServiceUnits value) = do
  exists <- readProposal conn account
  case exists of
    Just proposal -> do
      let updatedProposal = proposal {_proposalServiceUnits = value}
      runBeamSqliteDebug putStrLn conn $
        runUpdate $ save (proposalDb ^. proposals) updatedProposal
    Nothing -> print $ "Error: Account `" ++ T.unpack name ++ "` Doesn't Exist"
