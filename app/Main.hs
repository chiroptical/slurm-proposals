module Main where

import Options.Applicative (execParser)
import Dispatch (dispatch)
import Config
import Control.Monad.Trans.Reader

main :: IO ()
main = do
  config <- makeConfig
  runReaderT dispatch config
