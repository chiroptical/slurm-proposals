module Type.Database where

import           Control.Monad.Trans.Except (ExceptT)
import           Data.Text                  (Text)
import           Type.Frontend              (Proposal)

type DatabaseT = ExceptT DatabaseError

data DatabaseError
  = AccountDoesntExist Text
  | AccountIdDoesntExist Int
  | ProposalAlreadyExists Proposal
  | ProposalDoesntExist Text
  | ProposalIdDoesntExist Int
  | PurchasedUnitIdDoesntExist Int
  | StatisticIdDoesntExist Int
  | InternalError Text
  deriving (Show, Eq)
