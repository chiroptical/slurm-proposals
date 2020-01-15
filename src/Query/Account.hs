{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Query.Account where

import           Database
import           Table.Account
import           Type.Backend
import           Type.Frontend                   (Account (..))

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite.Connection (Sqlite)

selectAllAccounts ::
     BeamSqlBackend be
  => Q be ProposalsDb QBaseScope (AccountT (QExpr be QBaseScope))
selectAllAccounts = all_ (_proposalsAccount proposalsDb)

toAccount :: Account_ -> Account
toAccount Account_ { _accountName = name
                   , _accountOwner = owner
                   , _accountDepartment = dept
                   } = Account name owner dept

getAccount :: MonadBeam Sqlite m => Text -> m (Either BackendError Account)
getAccount name = (fmap . fmap) toAccount (getAccount_ name)

getAccount_ :: MonadBeam Sqlite m => Text -> m (Either BackendError Account_)
getAccount_ name = do
  mAccount_ <-
    runSelectReturningOne $
    select $ do
      account <- selectAllAccounts
      guard_ (_accountName account ==. val_ name)
      pure account
  pure $
    case mAccount_ of
      Nothing       -> Left $ AccountDoesntExist name
      Just account_ -> Right account_

getAccountById :: MonadBeam Sqlite m => Int -> m (Either BackendError Account)
getAccountById id = (fmap . fmap) toAccount (getAccountById_ id)

getAccountById_ :: MonadBeam Sqlite m => Int -> m (Either BackendError Account_)
getAccountById_ id = do
  account_ <-
    runSelectReturningOne $
    select $ do
      account <- selectAllAccounts
      guard_ (_accountId account ==. val_ id)
      pure account
  pure $
    case account_ of
      Nothing      -> Left $ AccountIdDoesntExist id
      Just account -> Right account

insertAccount ::
     MonadBeam Sqlite m => Account -> m (Either BackendError Account_)
insertAccount Account { accountName = name
                      , accountOwner = owner
                      , accountDepartment = dept
                      } = do
  runInsert $
    insert (_proposalsAccount proposalsDb) $
    insertExpressions [Account_ default_ (val_ name) (val_ owner) (val_ dept)]
  getAccount_ name
