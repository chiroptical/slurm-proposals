module Type.Frontend where

import           Data.Time.LocalTime
import           Table.Proposal      (NotificationPercent)

import           Data.Text           (Text)

data Account =
  Account
    { accountName       :: Text
    , accountOwner      :: Text
    , accountDepartment :: Text
    }
  deriving (Eq, Show)

data Proposal =
  Proposal
    { proposalServiceUnits        :: Int
    , proposalExpirationDate      :: LocalTime
    , proposalNotificationPercent :: NotificationPercent
    , proposalLocked              :: Bool
    , proposalAccount             :: Account
    }
  deriving (Eq, Show)

data PurchasedUnit =
  PurchasedUnit
    { purchasedUnitServiceUnits   :: Int
    , purchasedUnitExpirationDate :: LocalTime
    , purchasedUnitConsumed       :: Bool
    , purchasedUnitAccount        :: Account
    }
  deriving (Eq, Show)

data Statistic =
  Statistic
    { statisticUnusedServiceUnits :: Int
    , statisticAccount            :: Account
    }
  deriving (Eq, Show)
