{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import Data.Text (Text)
import qualified Data.Text as T

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import Database.Beam.Sqlite.Syntax
import Database.Beam.Backend.SQL
import Database.SQLite.Simple (Connection)

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import           Control.Monad                  ( void )

data NotificationPercent =
    Zero
  | TwentyFive
  | Fifty
  | SeventyFive
  | Hundred
  deriving (Eq, Ord, Show, Read)

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be NotificationPercent where
  defaultSqlDataType = defaultSqlDataType . fmap (T.pack . show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be NotificationPercent where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite NotificationPercent where
  fromBackendRow = read . T.unpack <$> fromBackendRow

notificationPercentDataType :: DataType Sqlite NotificationPercent
notificationPercentDataType = DataType sqliteTextType

newtype Account =
  Account
    { _account :: Text
    } deriving (Show, Eq)

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Account where
  defaultSqlDataType = defaultSqlDataType . fmap _account

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Account where
  sqlValueSyntax = sqlValueSyntax . _account

instance FromBackendRow Sqlite Account where
  fromBackendRow = Account <$> fromBackendRow

accountDataType :: DataType Sqlite Account
accountDataType = DataType sqliteTextType

newtype ServiceUnits =
  ServiceUnits
    { _serviceUnits :: Int
    } deriving (Show, Eq)

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be ServiceUnits where
  defaultSqlDataType = defaultSqlDataType . fmap _serviceUnits

instance HasSqlValueSyntax be Int => HasSqlValueSyntax be ServiceUnits where
  sqlValueSyntax = sqlValueSyntax . _serviceUnits

instance FromBackendRow Sqlite ServiceUnits where
  fromBackendRow = ServiceUnits <$> fromBackendRow

serviceUnitsDataType :: DataType Sqlite ServiceUnits
serviceUnitsDataType = DataType intType

data ProposalT f =
  Proposal
    { _proposalId :: Columnar f Int
    , _proposalAccount :: Columnar f Account
    , _proposalServiceUnits :: Columnar f ServiceUnits
    , _proposalEndDate :: Columnar f LocalTime
    , _proposalNotificationPercent :: Columnar f NotificationPercent
    , _proposalLocked :: Columnar f Bool
    , _proposalsSubmitted :: Columnar f Int
    , _proposalServiceUnitsUnused :: Columnar f Int
    } deriving Generic

type Proposal = ProposalT Identity

deriving instance Show Proposal
deriving instance Eq Proposal

type ProposalId = PrimaryKey ProposalT Identity

instance Beamable ProposalT

instance Table ProposalT where
  data PrimaryKey ProposalT f = ProposalId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ProposalId . _proposalId

newtype ProposalDb f =
  ProposalDb
    { _proposals :: f (TableEntity ProposalT)
    }
  deriving (Generic, Database be)

proposalDb :: DatabaseSettings Sqlite ProposalDb
proposalDb = unCheckDatabase $ evaluateDatabase initialSetupStep

initialSetup :: Migration Sqlite (CheckedDatabaseSettings Sqlite ProposalDb)
initialSetup = ProposalDb <$>
  (createTable "proposals" $
    Proposal
      { _proposalId = field "id" int notNull unique
      , _proposalAccount = field "account" accountDataType notNull
      , _proposalServiceUnits = field "serviceUnits" serviceUnitsDataType notNull
      , _proposalEndDate = field "endDate" timestamptz notNull
      , _proposalNotificationPercent = field "notificationPercent" notificationPercentDataType notNull
      , _proposalLocked = field "locked" boolean notNull
      , _proposalsSubmitted = field "proposalsSubmitted" int notNull
      , _proposalServiceUnitsUnused = field "serviceUnitsUnused" int notNull
      }
  )

initialSetupStep :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite ProposalDb)
initialSetupStep = migrationStep "initial_setup" (const initialSetup)

allowDestructive :: Monad m => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks { runIrreversibleHook = pure True }

migrateDb :: Connection -> IO (Maybe (CheckedDatabaseSettings Sqlite ProposalDb))
migrateDb conn =
  runBeamSqliteDebug putStrLn conn $
    bringUpToDateWithHooks allowDestructive migrationBackend initialSetupStep

createProposal :: Connection -> Account -> ServiceUnits -> IO ()
createProposal conn account units = do
  currTime <- getCurrentTime
  currTimeZone <- getCurrentTimeZone
  let endDate = addGregorianYearsClip 1 (utctDay currTime)
  void $ runBeamSqliteDebug putStrLn conn $
    runInsertReturningList $
      insert (_proposals proposalDb) $
      insertExpressions
        [ Proposal
          default_
          (val_ account)
          (val_ units)
          (val_ $ utcToLocalTime currTimeZone currTime { utctDay = endDate })
          (val_ Zero)
          (val_ False)
          (val_ 0)
          (val_ 0)
        ]
