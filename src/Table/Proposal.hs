{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Table.Proposal where

import           Data.Text                       (Text)
import           Data.Time.LocalTime
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite.Connection (Sqlite)
import           Table.Account                   (AccountT)

data NotificationPercent
  = Zero
  | TwentyFive
  | Fifty
  | SeventyFive
  | Hundred
  deriving (Eq, Ord, Enum, Show, Read)

instance HasSqlValueSyntax be Int =>
         HasSqlValueSyntax be NotificationPercent where
  sqlValueSyntax = sqlValueSyntax . fromEnum

instance FromBackendRow Sqlite NotificationPercent where
  fromBackendRow = toEnum <$> fromBackendRow

data ProposalT f =
  Proposal_
    { _proposalId                  :: C f Int
    , _proposalServiceUnits        :: C f Int
    , _proposalExpirationDate      :: C f LocalTime
    , _proposalNotificationPercent :: C f NotificationPercent
    , _proposalLocked              :: C f Bool
    , _proposalAccount             :: PrimaryKey AccountT f
    }
  deriving (Generic, Beamable)

type Proposal_ = ProposalT Identity

deriving instance Show Proposal_

instance Table ProposalT where
  data PrimaryKey ProposalT f = ProposalId (C f Int)
                                deriving (Generic, Beamable)
  primaryKey = ProposalId . _proposalId
