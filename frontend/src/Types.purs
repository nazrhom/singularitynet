module Types
  ( BondedPoolParams(..)
  , BondedStakingDatum(..)
  , BondedStakingAction(..)
  , PoolInfo(..)
  , AssetClass(..)
  , Entry(..)
  , StakingType(..)
  ) where

import Contract.Prelude

import ConstrIndices (class HasConstrIndices, defaultConstrIndices)
import Contract.Address (Address)
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Rational (Rational)
import Contract.PlutusData (class ToData, PlutusData(..), toData)
import Contract.Value (CurrencySymbol, TokenName)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import ToData (genericToData)
import Types.ByteArray (ByteArray)
import Types.UnbalancedTransaction (PaymentPubKeyHash)
import Utils (big)

newtype AssetClass = AssetClass
  { currencySymbol :: CurrencySymbol
  , tokenName :: TokenName
  }

derive instance Generic AssetClass _
derive instance Newtype AssetClass _
derive instance Eq AssetClass
instance HasConstrIndices AssetClass where
  constrIndices = defaultConstrIndices

instance ToData AssetClass where
  toData = genericToData

newtype PoolInfo = PoolInfo
  { stateNftCs :: CurrencySymbol
  , assocListCs :: CurrencySymbol
  , poolAddr :: Address
  }

derive instance Generic PoolInfo _
derive instance Newtype PoolInfo _
derive instance Eq PoolInfo

instance Show PoolInfo where
  show = genericShow

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

derive instance Generic BondedPoolParams _
derive instance Eq BondedPoolParams
derive instance Newtype BondedPoolParams _
instance HasConstrIndices BondedPoolParams where
  constrIndices = defaultConstrIndices

-- We copy the order of the fields from the Haskell implementation
instance ToData BondedPoolParams where
  toData (BondedPoolParams params) =
    Constr (big 0)
      [ toData params.iterations
      , toData params.start
      , toData params.end
      , toData params.userLength
      , toData params.bondingLength
      , toData params.interest
      , toData params.minStake
      , toData params.maxStake
      , toData params.admin
      , toData params.bondedAssetClass
      , toData params.nftCs
      , toData params.assocListCs
      ]

data BondedStakingDatum
  = StateDatum { maybeEntryName :: Maybe ByteArray, sizeLeft :: Natural }
  | EntryDatum { entry :: Entry }
  | AssetDatum

derive instance Generic BondedStakingDatum _
derive instance Eq BondedStakingDatum
instance HasConstrIndices BondedStakingDatum where
  constrIndices = defaultConstrIndices

instance ToData BondedStakingDatum where
  toData = genericToData

instance Show BondedStakingDatum where
  show = genericShow

data BondedStakingAction
  = AdminAct { sizeLeft :: Natural }
  | StakeAct
      { stakeAmount :: Natural
      , stakeHolder :: PaymentPubKeyHash
      }
  | WithdrawAct { stakeHolder :: PaymentPubKeyHash }
  | CloseAct

derive instance Generic BondedStakingAction _
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

derive instance Generic Entry _
derive instance Eq Entry
instance HasConstrIndices Entry where
  constrIndices = defaultConstrIndices

instance ToData Entry where
  toData = genericToData

instance Show Entry where
  show = genericShow

data StakingType = Bonded | Unbonded
