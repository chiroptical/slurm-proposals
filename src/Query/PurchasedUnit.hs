{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Query.PurchasedUnit where

import           Database
import           Query.Account
import           Table.Account
import           Table.PurchasedUnit
import           Type.Backend
import           Type.Frontend

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Database.Beam
import           Database.Beam.Backend.SQL

selectAllPurchasedUnits ::
     BeamSqlBackend be
  => Q be ProposalsDb QBaseScope (PurchasedUnitT (QExpr be QBaseScope))
selectAllPurchasedUnits = all_ (_proposalsPurchasedUnits proposalsDb)

toProposal :: (Account_, PurchasedUnit_) -> PurchasedUnit
toProposal (Account_ { _accountName = name
                     , _accountOwner = owner
                     , _accountDepartment = dept
                     }, PurchasedUnit_ { _purchasedUnitUnits = sus
                                       , _purchasedUnitExpirationDate = exp
                                       , _purchasedUnitConsumed = consumed
                                       }) =
  PurchasedUnit sus exp consumed (Account name owner dept)

getPurchasedUnits :: _ => Text -> m (Either BackendError [PurchasedUnit])
getPurchasedUnits = undefined

getPurchasedUnits_ ::
     _ => Text -> m (Either BackendError (Account_, [PurchasedUnit_]))
getPurchasedUnits_ name = do
  mAccount_ <- getAccount_ name
  case mAccount_ of
    Left err -> pure . Left $ err
    Right account_ -> do
      purchasedUnits_ <-
        runSelectReturningList $
        select $ do
          purchasedUnit <- selectAllPurchasedUnits
          guard_
            (_purchasedUnitAccount purchasedUnit ==.
             val_ (AccountId $ _accountId account_))
          pure purchasedUnit
      pure . Right $ (account_, purchasedUnits_)

getPurchasedUnitById :: _ => Int -> m (Either BackendError PurchasedUnit)
getPurchasedUnitById = undefined -- (fmap . fmap) toProposal . getProposalById_

getPurchasedUnitById_ ::
     _ => Int -> m (Either BackendError (Account_, PurchasedUnit_))
getPurchasedUnitById_ id = undefined

insertPurchasedUnit ::
     _ => PurchasedUnit -> m (Either BackendError (Account_, PurchasedUnit_))
insertPurchasedUnit PurchasedUnit { purchasedUnitServiceUnits = sus
                                  , purchasedUnitExpirationDate = exp
                                  , purchasedUnitConsumed = locked
                                  , purchasedUnitAccount = Account {accountName = name}
                                  } = undefined
