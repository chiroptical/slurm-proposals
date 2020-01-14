{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Queries where

import           Database
import           Datatypes
import           Errors
import           Table.Account
import           Table.Proposal
import           Table.PurchasedUnit
import           Table.Statistic

import           Control.Monad.Trans.Class (lift)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Database.Beam
import           Database.Beam.Backend.SQL

selectAllAccounts ::
     BeamSqlBackend be
  => Q be ProposalsDb QBaseScope (AccountT (QExpr be QBaseScope))
selectAllAccounts = all_ (_proposalsAccount proposalsDb)

insertAccount :: _ => Text -> Text -> m (Maybe Int)
insertAccount name owner = do
  runInsert $
    insert (_proposalsAccount proposalsDb) $
    insertExpressions [Account_ default_ (val_ name) (val_ owner)]
  getAccountId name

getAccount_ :: _ => Text -> m (Maybe Account_)
getAccount_ name =
  runSelectReturningOne $
  select $ do
    account <- selectAllAccounts
    guard_ (_accountName account ==. val_ name)
    pure account

getAccount :: _ => Text -> m (Maybe Account)
getAccount name = do
  account_ <- getAccount_ name
  case account_ of
    Nothing  -> pure Nothing
    Just acc -> pure . Just $ Account (_accountName acc) (_accountOwner acc)

getAccountId :: _ => Text -> m (Maybe Int)
getAccountId name =
  runSelectReturningOne $
  select $ do
    account <- selectAllAccounts
    guard_ (_accountName account ==. val_ name)
    pure $ _accountId account

getAccountById :: _ => Int -> m (Maybe Account_)
getAccountById id =
  runSelectReturningOne $
  select $ do
    account <- selectAllAccounts
    guard_ (_accountId account ==. val_ id)
    pure account

getProposal_ :: _ => Text -> m (Either DatabaseError Proposal_)
getProposal_ name = do
  id_ <- getAccountId name
  case id_ of
    Nothing -> pure . Left $ AccountDoesntExist name
    Just id' -> do
      proposal_ <-
        runSelectReturningOne $
        select $ do
          proposal <- all_ (_proposalsProposals proposalsDb)
          guard_ (_proposalAccount proposal ==. val_ (AccountId id'))
          pure proposal
      case proposal_ of
        Nothing       -> pure . Left $ ProposalDoesntExist name
        Just proposal -> pure . Right $ proposal

getProposal :: _ => Text -> m (Either DatabaseError Proposal)
getProposal name = do
  proposal_ <- getProposal_ name
  case proposal_ of
    Left l -> pure . Left $ l
    Right proposal -> do
      account_ <-
        runSelectReturningOne $
        lookup_ (_proposalsAccount proposalsDb) (_proposalAccount proposal)
      case account_ of
        Nothing -> pure . Left $ InternalError ""
        Just account ->
          pure . Right $
          Proposal
            (_proposalServiceUnits proposal)
            (_proposalExpirationDate proposal)
            (_proposalNotificationPercent proposal)
            (_proposalLocked proposal)
            (Account (_accountName account) (_accountOwner account))

getProposalById :: _ => Int -> m (Maybe Proposal_)
getProposalById id =
  runSelectReturningOne $
  select $ do
    proposal <- all_ (_proposalsProposals proposalsDb)
    guard_ (_proposalAccount proposal ==. val_ (AccountId id))
    pure proposal

getPurchasedUnits :: _ => Text -> m (Either Text [PurchasedUnit_])
getPurchasedUnits name = do
  id <- getAccountId name
  case id of
    Nothing -> pure . Left $ T.concat ["Accout ", name, " doesn't exist"]
    Just _id ->
      Right <$>
      runSelectReturningList
        (select $ do
           purchasedUnits <- all_ (_proposalsPurchasedUnits proposalsDb)
           guard_
             (_purchasedUnitAccount purchasedUnits ==. val_ (AccountId _id))
           pure purchasedUnits)

getStatistics :: _ => Text -> m (Either Text [Statistic_])
getStatistics name = do
  id <- getAccountId name
  case id of
    Nothing -> pure . Left $ T.concat ["Accout ", name, " doesn't exist"]
    Just _id ->
      Right <$>
      runSelectReturningList
        (select $ do
           statistic <- all_ (_proposalsStatistics proposalsDb)
           guard_ (_statisticAccount statistic ==. val_ (AccountId _id))
           pure statistic)
