module Types
  ( BondedPoolParams(..)
  , BondedStakingDatum(..)
  , BondedStakingAction(..)
  , AssetClass(..)
  , Entry(..)
  ) where

import Contract.Prelude
import Contract.Value (CurrencySymbol, TokenName)

import Data.Tuple (Tuple)
import Data.BigInt (BigInt)
import Data.Generic.Rep as G
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

import ConstrIndices (class HasConstrIndices, defaultConstrIndices)
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Rational (Rational)
import Contract.PlutusData (class ToData)

import ToData (genericToData)
import Types.ByteArray (ByteArray)
import Types.UnbalancedTransaction (PaymentPubKeyHash)

newtype AssetClass = AssetClass (Tuple CurrencySymbol TokenName)

derive instance G.Generic AssetClass _
derive instance Eq AssetClass
instance HasConstrIndices AssetClass where
  constrIndices = defaultConstrIndices

instance ToData AssetClass where
  toData = genericToData

-- TODO: Add missing `ToData` instances for POSIXTime and NatRatio.
newtype BondedPoolParams =
  BondedPoolParams
    { iterations :: Natural
    , start :: BigInt
    , end :: BigInt
    , userLength :: BigInt
    , bondingLength :: BigInt
    , interest :: Rational
    , minStake :: Natural
    , maxStake :: Natural
    , admin :: PaymentPubKeyHash
    , bondedAssetClass :: AssetClass
    , nftCs :: CurrencySymbol
    , assocListCs :: CurrencySymbol
    }

derive instance G.Generic BondedPoolParams _
derive instance Eq BondedPoolParams
derive instance Newtype BondedPoolParams _
instance HasConstrIndices BondedPoolParams where
  constrIndices = defaultConstrIndices

instance ToData BondedPoolParams where
  toData = genericToData

data BondedStakingDatum
  = StateDatum { maybeEntryName :: Maybe ByteArray }
  | EntryDatum { entry :: Entry }
  | AssetDatum

derive instance G.Generic BondedStakingDatum _
derive instance Eq BondedStakingDatum
instance HasConstrIndices BondedStakingDatum where
  constrIndices = defaultConstrIndices

instance ToData BondedStakingDatum where
  toData = genericToData

data BondedStakingAction
  = AdminAct { sizeLeft :: Natural }
  | StakeAct
      { stakeAmount :: Natural
      , stakeHolder :: PaymentPubKeyHash
      }
  | PWithdrawAct { stakeHolder :: PaymentPubKeyHash }
  | PCloseAct

derive instance G.Generic BondedStakingAction _
derive instance Eq BondedStakingAction
instance HasConstrIndices BondedStakingAction where
  constrIndices = defaultConstrIndices

instance ToData BondedStakingAction where
  toData = genericToData

newtype Entry =
  Entry
    { key :: ByteArray
    , sizeLeft :: BigInt
    , newDeposit :: BigInt
    , deposited :: BigInt
    , staked :: BigInt
    , rewards :: Rational
    , value :: Tuple BigInt Rational
    , next :: Maybe ByteArray
    }

derive instance G.Generic Entry _
derive instance Eq Entry
instance HasConstrIndices Entry where
  constrIndices = defaultConstrIndices

instance ToData Entry where
  toData = genericToData