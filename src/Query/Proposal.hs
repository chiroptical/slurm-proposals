{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Query.Proposal where

import           Database
import           Query.Account
import           Table.Account
import           Table.Proposal
import           Type.Backend
import           Type.Frontend

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite.Connection (Sqlite)

selectAllProposals ::
     Q Sqlite ProposalsDb QBaseScope (ProposalT (QExpr Sqlite QBaseScope))
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

getProposal :: MonadBeam Sqlite m => Text -> m (Either BackendError Proposal)
getProposal = (fmap . fmap) toProposal . getProposal_

getProposal_ ::
     MonadBeam Sqlite m => Text -> m (Either BackendError (Account_, Proposal_))
getProposal_ name = do
  mAccount_ <- accountByName_ name
  case mAccount_ of
    Left err -> pure $ Left err
    Right account_ -> do
      proposal_ <-
        runSelectReturningOne $
        select $ do
          proposal <- selectAllProposals
          guard_ (_proposalAccount proposal ==. val_ (primaryKey account_))
          pure proposal
      pure $
        case proposal_ of
          Nothing        -> Left $ ProposalDoesntExist name
          Just proposal_ -> Right (account_, proposal_)

getProposalById :: MonadBeam Sqlite m => Int -> m (Either BackendError Proposal)
getProposalById = (fmap . fmap) toProposal . getProposalById_

getProposalById_ ::
     MonadBeam Sqlite m => Int -> m (Either BackendError (Account_, Proposal_))
getProposalById_ id = do
  mProposal_ <-
    runSelectReturningOne $
    select $ do
      proposal <- selectAllProposals
      guard_ (_proposalId proposal ==. val_ id)
      pure proposal
  case mProposal_ of
    Nothing -> pure . Left $ ProposalIdDoesntExist id
    Just proposal_ -> do
      mAccount_ <- accountById_ (_proposalId proposal_)
      case mAccount_ of
        Left err       -> pure $ Left err
        Right account_ -> pure $ Right (account_, proposal_)

insertProposal ::
     MonadBeam Sqlite m
  => Proposal
  -> m (Either BackendError (Account_, Proposal_))
insertProposal Proposal { proposalServiceUnits = sus
                        , proposalExpirationDate = exp
                        , proposalNotificationPercent = notif
                        , proposalLocked = locked
                        , proposalAccount = Account {accountName = name}
                        } = do
  mAccount_ <- accountByName_ name
  case mAccount_ of
    Left err -> pure . Left $ err
    Right account_ -> do
      eProposal_ <- getProposal_ name
      case eProposal_ of
        Right _ -> pure . Left $ ProposalAlreadyExist name
        Left (ProposalDoesntExist _) -> do
          runInsert $
            insert (_proposalsProposals proposalsDb) $
            insertExpressions
              [ Proposal_
                  default_
                  (val_ sus)
                  (val_ exp)
                  (val_ notif)
                  (val_ locked)
                  (val_ $ primaryKey account_)
              ]
          getProposal_ name
        -- This shouldn't happen...
        Left err -> pure . Left $ err
