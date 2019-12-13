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
import Database.SQLite.Simple (Connection)

import Data.Time.LocalTime

data NotificationPercent =
    Zero
  | TwentyFive
  | Fifty
  | SeventyFive
  | Hundred
  deriving (Eq, Ord, Show)

data ProposalT f =
  Proposal
    { _proposalId :: Columnar f Integer
    , _proposalAccount :: Columnar f Text
    , _proposalServiceUnits :: Columnar f Integer
    , _proposalEndDate :: Columnar f LocalTime
    , _proposalNotificationProgress :: Columnar f Text
    , _proposalLocked :: Columnar f Bool
    , _proposalCount :: Columnar f Integer
    , _proposalServiceUnitsUnused :: Columnar f Integer
    } deriving Generic

type Proposal = ProposalT Identity

deriving instance Show Proposal
deriving instance Eq Proposal

type ProposalId = PrimaryKey ProposalT Identity

instance Beamable ProposalT

instance Table ProposalT where
  data PrimaryKey ProposalT f = ProposalId (Columnar f Integer)
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
      , _proposalAccount = field "account" (varchar Nothing) notNull
      , _proposalServiceUnits = field "serviceUnits" int notNull
      , _proposalEndDate = field "endDate" timestamptz notNull
      , _proposalNotificationProgress = field "notificationProgress" (varchar Nothing) notNull
      , _proposalLocked = field "locked" boolean notNull
      , _proposalCount = field "count" int notNull
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
createProposal conn (Account account) (ServiceUnits units) = do
  currTime <- getCurrentTime
  let endDate = addGregorianYearsClip 1 (utctDay currTime)
  undefined
