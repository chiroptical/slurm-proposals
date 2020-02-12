module Type.Database where

import           Control.Monad.Trans.Except (ExceptT)
import           Data.Text                  (Text)

type DatabaseT = ExceptT DatabaseError

data DatabaseError
  = AccountDoesntExist Text
  | AccountIdDoesntExist Int
  | ProposalAlreadyExists Text
  | ProposalDoesntExist Text
  | ProposalIdDoesntExist Int
  | PurchasedUnitIdDoesntExist Int
  | StatisticIdDoesntExist Int
  | InternalError Text
  deriving (Show, Eq)
