{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Database
import Database.SQLite.Simple (open)

example :: IO ()
example = do
  conn <- open "test.db"
  migrateDb conn
  createProposal conn (Account "bmooreii") (ServiceUnits 10000)
