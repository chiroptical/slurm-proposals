{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database

import           Control.Monad.IO.Class
import           Database.SQLite.Simple (open)

main :: IO ()
main = do
  conn <- liftIO $ open "test.db"
  liftIO $ migrateDb conn
  liftIO $ createProposal conn "bmooreii" "bmooreii" 10000
  result <- liftIO $ readProposal conn "bmooreii"
  print result
