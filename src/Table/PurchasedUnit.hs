{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Table.PurchasedUnit where

import           Data.Text           (Text)
import           Data.Time.LocalTime
import           Database.Beam
import           Table.Account       (AccountT)

data PurchasedUnitT f =
  PurchasedUnit
    { _purchasedUnitId             :: C f Int
    , _purchasedUnitUnits          :: C f Int
    , _purchasedUnitExpirationDate :: C f LocalTime
    , _purchasedUnitAccount        :: PrimaryKey AccountT f
    }
  deriving (Generic, Beamable)

type PurchasedUnit = PurchasedUnitT Identity

deriving instance Show PurchasedUnit

instance Table PurchasedUnitT where
  data PrimaryKey PurchasedUnitT f = PurchasedUnitId (C f Int)
                                     deriving (Generic, Beamable)
  primaryKey = PurchasedUnitId . _purchasedUnitId
