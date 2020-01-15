module Main where

import Options.Applicative
import Data.Semigroup ((<>))

newtype Account = Account String deriving Show

accountP :: Parser Account
accountP = Account <$> accountNameOpt

accountNameOpt = strOption (help "An account" <> long "account" <> metavar "ACCOUNT")

opts :: ParserInfo Account
opts = info accountP (progDesc "something here..." <> header "what is this?")

main :: IO ()
main = do
  options <- execParser opts
  print options
