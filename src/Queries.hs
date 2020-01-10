{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Queries where

import           Database
import           Table.Account
import           Table.Proposal
import           Table.PurchasedUnit
import           Table.Statistic

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Database.Beam
import           Database.Beam.Backend.SQL

insertAccount :: _ => Text -> Text -> m (Maybe Int)
insertAccount name owner = do
  runInsert $
    insert (_proposalsAccount proposalsDb) $
    insertExpressions [Account_ default_ (val_ name) (val_ owner)]
  runSelectReturningOne $
    select $ do
      account <- all_ (_proposalsAccount proposalsDb)
      guard_ (_accountName account ==. val_ name)
      pure $ _accountId account

getAccount :: _ => Text -> m (Maybe Account_)
getAccount name =
  runSelectReturningOne $
  select $ do
    account <- all_ (_proposalsAccount proposalsDb)
    guard_ (_accountName account ==. val_ name)
    pure account

getAccountId :: _ => Text -> m (Maybe Int)
getAccountId name =
  runSelectReturningOne $
  select $ do
    account <- all_ (_proposalsAccount proposalsDb)
    guard_ (_accountName account ==. val_ name)
    pure $ _accountId account

getProposal :: _ => Text -> m (Either Text Proposal_)
getProposal name = do
  id <- getAccountId name
  case id of
    Nothing -> pure . Left $ T.concat ["Accout ", name, " doesn't exist"]
    Just _id -> do
      result <-
        runSelectReturningOne $
        select $ do
          proposal <- all_ (_proposalsProposals proposalsDb)
          guard_ (_proposalAccount proposal ==. val_ (AccountId _id))
          pure proposal
      case result of
        Just _result -> pure . Right $ _result
        Nothing ->
          pure . Left $ T.concat ["Account ", name, " doesn't have a proposal"]

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
