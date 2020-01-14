module Errors where

import           Data.Text (Text)

data DatabaseError
  = AccountDoesntExist Text
  | ProposalDoesntExist Text
  | InternalError Text
