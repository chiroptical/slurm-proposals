{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Table.Account where

import           Data.Text     (Text)
import           Database.Beam

deriving instance Show (PrimaryKey AccountT Identity)

data AccountT f =
  Account_
    { _accountId         :: C f Int
    , _accountName       :: C f Text
    , _accountOwner      :: C f Text
    }
  deriving (Generic, Beamable)

type Account_ = AccountT Identity

deriving instance Show Account_

deriving instance Eq Account_

instance Table AccountT where
  data PrimaryKey AccountT f = AccountId (C f Int)
                               deriving (Generic, Beamable)
  primaryKey = AccountId . _accountId
