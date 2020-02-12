module Main where

import           CLI                        (Options (..), Subcommand (..),
                                             opts)
import           Options.Applicative        (execParser)

import           Database                   (makeTables)
import           Database.SQLite.Simple     (open)

import           Config                     (Config (..))

import           Query.Account              (insertAccount)
import           Query.Proposal             (insertProposal)
import           Table.Proposal             (NotificationPercent (Zero))
import           Type.Frontend              (Account (..), Proposal (..))

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Reader
import           Data.Time.Calendar         (addDays)
import           Data.Time.LocalTime        (LocalTime, ZonedTime (..),
                                             getZonedTime, localDay)
import           Database.Beam.Schema       (primaryKey)
import           Database.Beam.Sqlite       (runBeamSqliteDebug)

type SlurmProp = ReaderT Config IO

getExpirationDate :: IO LocalTime
getExpirationDate = do
  zonedTime_ <- getZonedTime
  let localTime = zonedTimeToLocalTime zonedTime_
      expirationDay = addDays 365 . localDay $ localTime
  return . zonedTimeToLocalTime $
    zonedTime_ {zonedTimeToLocalTime = localTime {localDay = expirationDay}}

dispatch :: SlurmProp ()
dispatch = do
  Config {databaseConnection = conn, cliOptions = opts} <- ask
  case optionsSubcommand opts of
    Insert -> do
      let account = Account (optionsAccount opts) (optionsOwner opts)
      expirationLocalTime <- liftIO getExpirationDate
      result <-
        liftIO . runBeamSqliteDebug print conn . runExceptT $ do
          account_ <-
            insertAccount $ Account (optionsAccount opts) (optionsOwner opts)
          insertProposal (primaryKey account_) $
            Proposal
              (optionsServiceUnits opts)
              expirationLocalTime
              Zero
              False
              account
      case result of
        Left msg -> liftIO . print $ msg
        Right _  -> pure ()
      pure ()

main :: IO ()
main = do
  cli <- execParser opts
  conn <- open "proposals.db"
  -- TODO: This should be makeTablesIfNotExists
  makeTables conn
  runReaderT dispatch (Config conn cli)
