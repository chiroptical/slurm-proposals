module Config where

import           CLI                    (Options)
import           Database.SQLite.Simple (Connection)

data Config =
  Config
    { databaseConnection :: Connection
    , cliOptions         :: Options
    }
