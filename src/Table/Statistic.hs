{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Table.Statistic where

import           Data.Text           (Text)
import           Data.Time.LocalTime (LocalTime)
import           Database.Beam
import           Table.Account       (AccountT)

data StatisticT f =
  Statistic_
    { _statisticId                 :: C f Int
    , _statisticUnusedServiceUnits :: C f Int
    , _statisticExpirationDate     :: C f LocalTime
    , _statisticAccount            :: PrimaryKey AccountT f
    }
  deriving (Generic, Beamable)

type Statistic_ = StatisticT Identity

deriving instance Show Statistic_

instance Table StatisticT where
  data PrimaryKey StatisticT f = StatisticId (C f Int)
                                 deriving (Generic, Beamable)
  primaryKey = StatisticId . _statisticId
