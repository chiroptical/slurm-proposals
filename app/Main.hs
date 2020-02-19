module Main where

import           CLI                        (Options (..), opts)
import           Options.Applicative        (execParser)

import           Database                   (makeTablesIfNotExists)
import           Database.SQLite.Simple     (open)

import           Config                     (Config (..))

import           Query.Account              (insertAccountIfNotExists, accountByName_)
import           Query.Proposal             (insertProposal, proposalByName_)
import           Table.Proposal             (NotificationPercent (Zero))
import           Type.Frontend              (Account (..), Proposal (..))

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Reader
import           Data.Time.Calendar         (addDays)
import           Data.Time.LocalTime        (LocalTime, ZonedTime (..),
                                             getZonedTime, localDay)
import           Data.Maybe                     ( fromMaybe )
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
  case opts of
    Insert name owner units -> do
      let account = Account name owner
      expirationLocalTime <- liftIO getExpirationDate
      result <-
        liftIO . runBeamSqliteDebug print conn . runExceptT $ do
          account_ <-
            insertAccountIfNotExists account
          insertProposal account_ $
            Proposal
              units
              expirationLocalTime
              Zero
              False
              account
      liftIO $ case result of
        Left msg -> print msg
        Right prop -> print prop
    Get name -> do
      result <-
        liftIO . runBeamSqliteDebug print conn . runExceptT $ do
          account_ <- accountByName_ name
          proposalByName_ account_
      liftIO $ case result of
        Left msg -> print msg
        Right prop -> print prop

-- TODO: Database operations should happen inside of a transaction
main :: IO ()
main = do
  cli <- execParser opts
  conn <- open "proposals.db"
  makeTablesIfNotExists conn
  runReaderT dispatch (Config conn cli)
