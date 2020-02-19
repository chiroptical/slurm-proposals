module CLI where

import           Data.Semigroup                  ((<>))
import           Data.String                     (IsString (..))
import           Data.Text                       (Text)
import           Options.Applicative
import           Options.Applicative.Help.Pretty (text)

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

serviceUnits :: Parser Int
serviceUnits =
  option auto $
  long "serviceUnits" <> short 's' <> metavar "NUMBER" <>
  help "The number of service units to insert"

data Options =
  Insert
    { optionsAccount      :: Text
    , optionsOwner        :: Text
    , optionsServiceUnits :: Int
    }
  | Get { optionsAccount      :: Text }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  subparser
    (  command "insert" (info (Insert <$> account <*> owner <*> serviceUnits) (progDesc "Insert a new proposal"))
    <> command "get" (info (Get <$> account) (progDesc "Get a proposal by name"))
    )

opts :: ParserInfo Options
opts = info (optionsParser <**> helper) (fullDesc <> header "" <> progDesc "")
