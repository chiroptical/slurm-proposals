{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Query.Account where

import           Common                          (fromMaybeE)
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
                   } = Account name owner

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

accountByName :: MonadBeam Sqlite m => Text -> m (Either BackendError Account)
accountByName name = (fmap . fmap) toAccount (accountByName_ name)

accountByName_ :: MonadBeam Sqlite m => Text -> m (Either BackendError Account_)
accountByName_ name =
  fromMaybeE (AccountDoesntExist name) <$>
  runSelectReturningOne (selectAccountsWhere _accountName name)

accountById :: MonadBeam Sqlite m => Int -> m (Either BackendError Account)
accountById id = (fmap . fmap) toAccount (accountById_ id)

accountById_ :: MonadBeam Sqlite m => Int -> m (Either BackendError Account_)
accountById_ id =
  fromMaybeE (AccountIdDoesntExist id) <$>
  runSelectReturningOne (selectAccountsWhere _accountId id)

insertAccount ::
     MonadBeam Sqlite m => Account -> m (Either BackendError Account_)
insertAccount Account { accountName = name
                      , accountOwner = owner
                      } = do
  runInsert $
    insert (_proposalsAccount proposalsDb) $
    insertExpressions [Account_ default_ (val_ name) (val_ owner)]
  accountByName_ name
