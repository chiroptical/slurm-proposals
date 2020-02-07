module Config where

import Database.SQLite.Simple
import CLI
import Options.Applicative
import Database

data Config =
  Config
    { databaseConnection :: Connection
    , cliOptions :: Options
    }

makeConfig :: IO Config
makeConfig = do
  cli <- execParser opts
  conn <- open "proposals.db"
  makeTables conn
  return $ Config conn cli
