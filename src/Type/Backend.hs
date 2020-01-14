module Type.Backend where

import           Data.Text (Text)

data BackendError
  = AccountDoesntExist Text
  | AccountIdDoesntExist Int
  | ProposalDoesntExist Text
  | ProposalIdDoesntExist Int
  | InternalError Text
