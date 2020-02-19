{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Query.Proposal where

import           Common
import           Database
import           Query.Account
import           Table.Account
import           Table.Proposal
import           Type.Database
import           Type.Frontend

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite.Connection (Sqlite)

import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Except

selectAllProposals ::
     Q Sqlite ProposalsDb QBaseScope (ProposalT (QExpr Sqlite QBaseScope))
selectAllProposals = all_ (_proposalsProposals proposalsDb)

toProposal :: (Account_, Proposal_) -> Proposal
toProposal (Account_ {_accountName = name, _accountOwner = owner}, Proposal_ { _proposalServiceUnits = sus
                                                                             , _proposalExpirationDate = exp
                                                                             , _proposalNotificationPercent = notif
                                                                             , _proposalLocked = locked
                                                                             }) =
  Proposal sus exp notif locked (Account name owner)

selectProposalWhere :: MonadBeam Sqlite m => Account_ -> m (Maybe Proposal_)
selectProposalWhere account_ =
  runSelectReturningOne . select $ do
    proposal <- selectAllProposals
    guard_ (_proposalAccount proposal ==. val_ (primaryKey account_))
    pure proposal

proposalByName :: MonadBeam Sqlite m => Account_ -> DatabaseT m Proposal
proposalByName = fmap toProposal . proposalByName_

proposalByName_ ::
     MonadBeam Sqlite m => Account_ -> DatabaseT m (Account_, Proposal_)
proposalByName_ acct_ = do
  proposal_ <-
    ExceptT $
    fromMaybeE (ProposalDoesntExist $ _accountName acct_) <$> selectProposalWhere acct_
  return (acct_, proposal_)

proposalById :: MonadBeam Sqlite m => Account_ -> ExceptT DatabaseError m Proposal
proposalById = fmap toProposal . proposalById_

proposalById_ ::
     MonadBeam Sqlite m => Account_ -> ExceptT DatabaseError m (Account_, Proposal_)
proposalById_ acct_ = do
  proposal_ <-
    ExceptT $
    fromMaybeE (ProposalIdDoesntExist (_accountId acct_)) <$> selectProposalWhere acct_
  return (acct_, proposal_)

insertProposal ::
     MonadBeam Sqlite m
  => Account_
  -> Proposal
  -> ExceptT DatabaseError m (Account_, Proposal_)
insertProposal acct_ Proposal { proposalServiceUnits = sus
                             , proposalExpirationDate = exp
                             , proposalNotificationPercent = notif
                             , proposalLocked = locked
                               } = do
  proposal_ <- selectProposalWhere acct_
  case proposal_ of
    Nothing -> do
      runInsert $
        insert (_proposalsProposals proposalsDb) $
        insertExpressions
          [ Proposal_
              default_
              (val_ sus)
              (val_ exp)
              (val_ notif)
              (val_ locked)
              (val_ $ primaryKey acct_)
          ]
      proposalByName_ acct_
    Just proposal -> throwE . ProposalAlreadyExists $ toProposal (acct_, proposal)
