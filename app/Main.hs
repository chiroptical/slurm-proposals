module Main where

import           Data.Semigroup                  ((<>))
import           Data.String                     (IsString (..))
import           Data.Text                       (Text)
import           Options.Applicative
import           Options.Applicative.Help.Pretty (text)

data Subcommand
  = Insert
  | Modify
  | Add
  | Get
  | Reset
  | Release
  | Invalid
  deriving (Show)

instance IsString Subcommand where
  fromString "insert"  = Insert
  fromString "modify"  = Modify
  fromString "add"     = Add
  fromString "get"     = Get
  fromString "reset"   = Reset
  fromString "release" = Release
  fromString _         = Invalid

subcommandParser :: Parser Subcommand
subcommandParser =
  argument str $
  metavar "COMMAND" <>
  helpDoc
    (Just
       (text
          "\n\
          \    Valid commands: \n\
          \      insert:  Insert a new proposal\n\
          \      modify:  Modify service unit limit\n\
          \      add:     Add additional service units\n\
          \      get:     Get the current limit\n\
          \      reset:   Reset usage in Slurm\n\
          \      release: Release hold in Slurm"))

data Options =
  Options
    { optionsAccount      :: Text
    , optionsServiceUnits :: Int
    , optionsSubcommand   :: Subcommand
    }
  deriving (Show)

account :: Parser Text
account =
  strOption $
  long "account" <> short 'a' <> metavar "STRING" <>
  help "The account name for the proposal"

optionsParser :: Parser Options
optionsParser = Options <$> account <*> serviceUnits <*> subcommandParser
  where
    serviceUnits =
      option auto $
      long "serviceUnits" <> short 's' <> metavar "NUMBER" <> value 10000 <>
      showDefault <>
      help "The number of service units to insert"

opts :: ParserInfo Options
opts = info (optionsParser <**> helper) (fullDesc <> header "" <> progDesc "")

main :: IO ()
main = do
  options <- execParser opts
  print options
