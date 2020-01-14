{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Query.Proposal where

import           Database
import           Query.Account
import           Table.Account
import           Table.Proposal
import           Type.Backend
import           Type.Frontend

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Database.Beam
import           Database.Beam.Backend.SQL

selectAllProposals ::
     BeamSqlBackend be
  => Q be ProposalsDb QBaseScope (ProposalT (QExpr be QBaseScope))
selectAllProposals = all_ (_proposalsProposals proposalsDb)

toProposal :: (Account_, Proposal_) -> Proposal
toProposal (Account_ { _accountName = name
                     , _accountOwner = owner
                     , _accountDepartment = dept
                     }, Proposal_ { _proposalServiceUnits = sus
                                  , _proposalExpirationDate = exp
                                  , _proposalNotificationPercent = notif
                                  , _proposalLocked = locked
                                  }) =
  Proposal sus exp notif locked (Account name owner dept)

getProposal :: _ => Text -> m (Either BackendError Proposal)
getProposal name = (fmap . fmap) toProposal (getProposal_ name)

getProposal_ :: _ => Text -> m (Either BackendError (Account_, Proposal_))
getProposal_ name = do
  mAccount_ <- getAccount_ name
  case mAccount_ of
    Left err -> pure $ Left err
    Right account_ -> do
      proposal_ <-
        runSelectReturningOne $
        select $ do
          proposal <- selectAllProposals
          guard_
            (_proposalAccount proposal ==.
             val_ (AccountId $ _accountId account_))
          pure proposal
      pure $
        case proposal_ of
          Nothing        -> Left $ ProposalDoesntExist name
          Just proposal_ -> Right (account_, proposal_)
