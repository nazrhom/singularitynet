module SingularityNet.Types where

import Contract.Prelude
import Contract.Address (PaymentPubKeyHash)
import Contract.PlutusData (class FromData, class ToData)

newtype BondedStakingState = BondedStakingState (Array PaymentPubKeyHash)

derive instance Generic BondedStakingState _
derive instance Newtype BondedStakingState _
derive newtype instance Eq BondedStakingState
derive newtype instance Ord BondedStakingState
derive newtype instance FromData BondedStakingState
derive newtype instance ToData BondedStakingState

instance Show BondedStakingState where
  show = genericShow