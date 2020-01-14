{-# LANGUAGE OverloadedStrings #-}

import           Database                        (makeTables)
import           Query.Account                   (insertAccount)
import           Table.Account                   (AccountT (..), Account_)
import           Type.Frontend                   (Account (..))

import           Database.Beam.Sqlite.Connection
import           Database.SQLite.Simple          (close, open)

import           System.Directory                (doesFileExist, removeFile)
import           Test.Hspec

-- Fixtures
testAccount :: Account
testAccount = Account "test" "test" "test"

firstAccount :: Account_
firstAccount =
  Account_
    1
    (accountName testAccount)
    (accountOwner testAccount)
    (accountDepartment testAccount)

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
        account_ <-
          runBeamSqliteDebug putStrLn conn . insertAccount $ testAccount
        account_ `shouldNotSatisfy` \a_ -> null a_
        account_ `shouldBe` Right firstAccount
      it "should insert a proposal" pending
      it
        "should return Left AccountDoesNotExist when inserting proposal"
        pending
  close conn
