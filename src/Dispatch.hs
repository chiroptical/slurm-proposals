module Dispatch where

import           Data.Text                      ( Text )
import           Query.Account                  ( insertAccount )
import           Type.Frontend
import           Database.SQLite.Simple         ( open )
import           Database.Beam.Sqlite
import           Database                       ( makeTables )
import           Config
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class         ( liftIO )
import           CLI

dispatch :: ReaderT Config IO ()
dispatch = do
  Config { databaseConnection = conn, cliOptions = opts } <- ask
  case optionsSubcommand opts of
    Insert -> do
      results <- liftIO $ runBeamSqlite conn . insertAccount $ Account
        (optionsAccount opts)
        (optionsOwner opts)
      pure ()
