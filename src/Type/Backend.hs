module Type.Backend where

import           Data.Text (Text)

data BackendError
  = AccountDoesntExist Text
  | AccountIdDoesntExist Int
  | ProposalAlreadyExist Text
  | ProposalDoesntExist Text
  | ProposalIdDoesntExist Int
  | PurchasedUnitIdDoesntExist Int
  | StatisticIdDoesntExist Int
  | InternalError Text
  deriving (Show, Eq)
