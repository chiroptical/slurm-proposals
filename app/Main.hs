{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Database.SQLite.Simple (open)

main :: IO ()
main = do
  putStrLn "place holder"
  -- conn <- liftIO $ open "test.db"
  -- liftIO $ migrateDb conn
  -- liftIO $
  --   insertNewProposal
  --     conn
  --     (Account "bmooreii")
  --     (AccountOwner "bmooreii")
  --     (ServiceUnits 10000)
  -- result <- liftIO $ maybeGetProposal conn (Account "bmooreii")
  -- print result
