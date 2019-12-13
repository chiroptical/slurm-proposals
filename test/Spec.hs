{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import           Test.Hspec
import           Database
import           Database.SQLite.Simple         ( open )
import           System.Directory               ( removeFile
                                                , doesFileExist
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Class      ( lift )

newtype FileExists =
  FileExists
    { doesExist :: Bool
    } deriving (Eq, Show)

class MonadIO m => MonadFileExists m where
  mFileExists :: String -> m FileExists

instance MonadFileExists IO where
  mFileExists fileName = FileExists <$> doesFileExist fileName

instance MonadFileExists (IdentityT IO) where
  mFileExists fileName = lift $ FileExists <$> doesFileExist fileName

-- If the test database exists, remove it
-- try to migrate it
-- test database should exist
tryMigrateDb :: MonadFileExists m => m Bool
tryMigrateDb = do
  let fileName = "test.db"
  FileExists remove <- mFileExists fileName
  if remove then liftIO $ removeFile fileName else pure ()
  conn <- liftIO $ open "test.db"
  liftIO $ migrateDb conn
  FileExists exists <- mFileExists fileName
  return exists

main :: IO ()
main = hspec $ describe "Database Operations" $ do
  it "should create test.db file" $
    runIdentityT tryMigrateDb `shouldReturn` True
  it "test.db should have the correct schema" pending
  it "should enter an example"                pending
