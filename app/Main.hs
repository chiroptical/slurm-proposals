module Main where

import           Options.Applicative            ( execParser )
import           CLI                            ( opts
                                                , Options(..)
                                                , Subcommand(..)
                                                )

import           Database.SQLite.Simple         ( open )
import           Database                       ( makeTables )

import           Config                         ( Config(..) )

import           Type.Frontend                  ( Account(..) )
import           Query.Account                  ( insertAccount )

import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class         ( liftIO )
import           Database.Beam.Sqlite           ( runBeamSqlite )

type SlurmProp = ReaderT Config IO

dispatch :: SlurmProp ()
dispatch = do
  Config { databaseConnection = conn, cliOptions = opts } <- ask
  case optionsSubcommand opts of
    Insert -> do
      results <- liftIO $ runBeamSqlite conn . insertAccount $ Account
        (optionsAccount opts)
        (optionsOwner opts)
      pure ()

main :: IO ()
main = do
  cli  <- execParser opts
  conn <- open "proposals.db"
  -- TODO: This should be makeTablesIfNotExists
  makeTables conn
  runReaderT dispatch (Config conn cli)
