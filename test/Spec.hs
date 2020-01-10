{-# LANGUAGE OverloadedStrings #-}

import           Database                        (makeTables)
import           Queries

import           Database.Beam.Sqlite.Connection
import           Database.SQLite.Simple          (close, open)

import           System.Directory                (doesFileExist, removeFile)
import           Test.Hspec

main :: IO ()
main = do
  exists <- doesFileExist "test.db"
  if exists
    then removeFile "test.db"
    else pure ()
  conn <- open "test.db"
  makeTables conn
  hspec $
    describe "Database Operations" $ do
      it "should insert an account" $ do
        accountId <-
          runBeamSqliteDebug putStrLn conn $ insertAccount "bmoore" "bmooreii"
        accountId `shouldNotBe` Nothing
      it "should do something else" pending
  close conn
