{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
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

proposalByName :: MonadBeam Sqlite m => Text -> DatabaseT m Proposal
proposalByName = fmap toProposal . proposalByName_

proposalByName_ ::
     MonadBeam Sqlite m => Text -> DatabaseT m (Account_, Proposal_)
proposalByName_ name = do
  account_ <- accountByName_ name
  proposal_ <-
    ExceptT $
    fromMaybeE (ProposalDoesntExist name) <$> selectProposalWhere account_
  return (account_, proposal_)

proposalById :: MonadBeam Sqlite m => Int -> ExceptT DatabaseError m Proposal
proposalById = fmap toProposal . proposalById_

proposalById_ ::
     MonadBeam Sqlite m => Int -> ExceptT DatabaseError m (Account_, Proposal_)
proposalById_ id = do
  account_ <- accountById_ id
  proposal_ <-
    ExceptT $
    fromMaybeE (ProposalIdDoesntExist id) <$> selectProposalWhere account_
  return (account_, proposal_)

exists :: MonadBeam Sqlite m => Text -> DatabaseT m (Account_, Proposal_)
exists name = do
  proposalByName_ name
  throwE $ ProposalAlreadyExists name

-- TODO: entityId should be passed to proposalBy... functions
insertProposal ::
     MonadBeam Sqlite m
  => PrimaryKey AccountT Identity
  -> Proposal
  -> ExceptT DatabaseError m (Account_, Proposal_)
insertProposal entityId Proposal { proposalServiceUnits = sus
                                 , proposalExpirationDate = exp
                                 , proposalNotificationPercent = notif
                                 , proposalLocked = locked
                                 , proposalAccount = Account {accountName = name}
                                 } =
  exists name `catchE` \case
    ProposalDoesntExist _ -> do
      runInsert $
        insert (_proposalsProposals proposalsDb) $
        insertExpressions
          [ Proposal_
              default_
              (val_ sus)
              (val_ exp)
              (val_ notif)
              (val_ locked)
              (val_ entityId)
          ]
      proposalByName_ name
    err@(ProposalAlreadyExists _) -> throwE err
