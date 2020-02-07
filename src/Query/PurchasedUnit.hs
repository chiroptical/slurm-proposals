{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TupleSections    #-}

module Query.PurchasedUnit where

import           Database
import           Query.Account
import           Table.Account
import           Table.PurchasedUnit
import           Type.Backend
import           Type.Frontend

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite.Connection (Sqlite)

selectAllPurchasedUnits ::
     Q Sqlite ProposalsDb QBaseScope (PurchasedUnitT (QExpr Sqlite QBaseScope))
selectAllPurchasedUnits = all_ (_proposalsPurchasedUnits proposalsDb)

toProposal :: (Account_, PurchasedUnit_) -> PurchasedUnit
toProposal (Account_ { _accountName = name
                     , _accountOwner = owner
                     }, PurchasedUnit_ { _purchasedUnitUnits = sus
                                       , _purchasedUnitExpirationDate = exp
                                       , _purchasedUnitConsumed = consumed
                                       }) =
  PurchasedUnit sus exp consumed (Account name owner)

getPurchasedUnits ::
     MonadBeam Sqlite m => Text -> m (Either BackendError [PurchasedUnit])
getPurchasedUnits name = do
  ePurchasedUnit_ <- getPurchasedUnits_ name
  case ePurchasedUnit_ of
    Left err -> pure . Left $ err
    Right (account_, purchasedUnits_) ->
      pure . Right $ toProposal <$> zip (repeat account_) purchasedUnits_

getPurchasedUnits_ ::
     MonadBeam Sqlite m
  => Text
  -> m (Either BackendError (Account_, [PurchasedUnit_]))
getPurchasedUnits_ name = do
  mAccount_ <- accountByName_ name
  case mAccount_ of
    Left err -> pure . Left $ err
    Right account_ ->
      fmap (Right . (account_, )) $
      runSelectReturningList $
      select $ do
        purchasedUnit <- selectAllPurchasedUnits
        guard_
          (_purchasedUnitAccount purchasedUnit ==. val_ (primaryKey account_))
        pure purchasedUnit

getPurchasedUnitById ::
     MonadBeam Sqlite m => Int -> m (Either BackendError PurchasedUnit)
getPurchasedUnitById = (fmap . fmap) toProposal . getPurchasedUnitById_

getPurchasedUnitById_ ::
     MonadBeam Sqlite m
  => Int
  -> m (Either BackendError (Account_, PurchasedUnit_))
getPurchasedUnitById_ id = do
  mPurchasedUnit_ <-
    runSelectReturningOne $
    select $ do
      purchasedUnit <- selectAllPurchasedUnits
      guard_ (_purchasedUnitId purchasedUnit ==. val_ id)
      pure purchasedUnit
  case mPurchasedUnit_ of
    Nothing -> pure . Left $ PurchasedUnitIdDoesntExist id
    Just purchasedUnit_ -> do
      let AccountId id = _purchasedUnitAccount purchasedUnit_
      fmap (, purchasedUnit_) <$> accountById_ id

insertPurchasedUnit ::
     MonadBeam Sqlite m
  => PurchasedUnit
  -> m (Either BackendError (Account_, PurchasedUnit_))
insertPurchasedUnit PurchasedUnit { purchasedUnitServiceUnits = sus
                                  , purchasedUnitExpirationDate = exp
                                  , purchasedUnitConsumed = locked
                                  , purchasedUnitAccount = Account {accountName = name}
                                  } = do
  eAccount_ <- accountByName_ name
  case eAccount_ of
    Left err -> pure . Left $ err
    Right account_ -> do
      runInsert $
        insert (_proposalsPurchasedUnits proposalsDb) $
        insertExpressions
          [ PurchasedUnit_
              default_
              (val_ sus)
              (val_ exp)
              (val_ locked)
              (val_ $ primaryKey account_)
          ]
      (fmap . fmap . fmap) last (getPurchasedUnits_ name)
