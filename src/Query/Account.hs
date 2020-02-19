{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}

module Query.Account where

import           Common                          (fromMaybeE)
import           Database
import           Table.Account
import           Type.Database
import           Type.Frontend                   (Account (..))

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite.Connection (Sqlite)

import           Control.Monad.Trans.Except

selectAllAccounts ::
     BeamSqlBackend be
  => Q be ProposalsDb QBaseScope (AccountT (QExpr be QBaseScope))
selectAllAccounts = all_ (_proposalsAccount proposalsDb)

toAccount :: Account_ -> Account
toAccount Account_ {_accountName = name, _accountOwner = owner} =
  Account name owner

selectAccountsWhere ::
     ( SqlValable a
     , HasQBuilder be
     , SqlEq (QGenExpr QValueContext be QBaseScope) a
     )
  => (AccountT (QExpr be QBaseScope) -> a)
  -> HaskellLiteralForQExpr a
  -> SqlSelect be (QExprToIdentity (AccountT (QExpr be QBaseScope)))
selectAccountsWhere accessor value =
  select $ filter_ (\acc -> accessor acc ==. val_ value) selectAllAccounts

accountByName :: MonadBeam Sqlite m => Text -> DatabaseT m Account
accountByName = fmap toAccount . accountByName_

accountByName_ :: MonadBeam Sqlite m => Text -> DatabaseT m Account_
accountByName_ name =
  ExceptT $
  fromMaybeE (AccountDoesntExist name) <$>
  runSelectReturningOne (selectAccountsWhere _accountName name)

accountById :: MonadBeam Sqlite m => Int -> DatabaseT m Account
accountById = fmap toAccount . accountById_

accountById_ :: MonadBeam Sqlite m => Int -> DatabaseT m Account_
accountById_ id =
  ExceptT $
  fromMaybeE (AccountIdDoesntExist id) <$>
  runSelectReturningOne (selectAccountsWhere _accountId id)

insertAccountIfNotExists ::
     MonadBeam Sqlite m => Account -> DatabaseT m Account_
insertAccountIfNotExists Account {accountName = name, accountOwner = owner} =
  accountByName_ name `catchE` \case
    AccountDoesntExist _ -> do
      runInsert $
        insert (_proposalsAccount proposalsDb) $
        insertExpressions [Account_ default_ (val_ name) (val_ owner)]
      accountByName_ name
