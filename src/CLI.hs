module CLI where

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
    , optionsOwner        :: Text
    , optionsServiceUnits :: Int
    , optionsSubcommand   :: Subcommand
    }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options <$> account <*> owner <*> serviceUnits <*> subcommandParser
  where
    serviceUnits :: Parser Int
    serviceUnits =
      option auto $
      long "serviceUnits" <> short 's' <> metavar "NUMBER" <> value 10000 <>
      showDefault <>
      help "The number of service units to insert"
    account :: Parser Text
    account =
      strOption $
      long "account" <> short 'a' <> metavar "STRING" <>
      help "The account owner's group name for the proposal"
    owner :: Parser Text
    owner =
      strOption $
      long "owner" <> short 'o' <> metavar "STRING" <>
      help "The account owner's user id for the proposal"

opts :: ParserInfo Options
opts = info (optionsParser <**> helper) (fullDesc <> header "" <> progDesc "")
