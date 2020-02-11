module Config where

import           Database.SQLite.Simple         ( Connection )
import           CLI                            ( Options )

data Config =
  Config
    { databaseConnection :: Connection
    , cliOptions :: Options
    }
