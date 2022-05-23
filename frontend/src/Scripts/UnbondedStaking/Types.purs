module UnbondedStaking.Types
  ( Entry(..)
  , InitialUnbondedParams(..)
  , StakingType
  , UnbondedPoolParams(..)
  , UnbondedStakingAction(..)
  , UnbondedStakingDatum(..)
  ) where

import Contract.Prelude

import ConstrIndices (class HasConstrIndices, defaultConstrIndices)
import Contract.Address (PaymentPubKeyHash)
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Rational (Rational)
import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Value (CurrencySymbol)
import Data.BigInt (BigInt)
import ToData (genericToData)
import Types (AssetClass)

-- TODO: Add missing `ToData` instances for POSIXTime and NatRatio.
newtype UnbondedPoolParams =
  UnbondedPoolParams
    { start :: BigInt
    , userLength :: BigInt
    , adminLength :: BigInt
    , bondingLength :: BigInt
    , interestLength :: BigInt
    , increments :: Natural
    , interest :: Rational
    , minStake :: Natural
    , maxStake :: Natural
    , admin :: PaymentPubKeyHash
    , unbondedAssetClass :: AssetClass
    , nftCs :: CurrencySymbol
    , assocListCs :: CurrencySymbol
    }

derive instance Generic UnbondedPoolParams _
derive instance Eq UnbondedPoolParams
derive instance Newtype UnbondedPoolParams _
instance HasConstrIndices UnbondedPoolParams where
  constrIndices = defaultConstrIndices

newtype InitialUnbondedParams = InitialUnbondedParams
  { start :: BigInt
  , userLength :: BigInt
  , adminLength :: BigInt
  , bondingLength :: BigInt
  , interestLength :: BigInt
  , increments :: Natural
  , interest :: Rational
  , minStake :: Natural
  , maxStake :: Natural
  , unbondedAssetClass :: AssetClass
  }

derive instance Generic InitialUnbondedParams _
derive instance Newtype InitialUnbondedParams _
derive instance Eq InitialUnbondedParams

instance Show InitialUnbondedParams where
  show = genericShow

-- We copy the order of the fields from the Haskell implementation
instance ToData UnbondedPoolParams where
  toData (UnbondedPoolParams params) =
    Constr zero
      [ toData params.start
      , toData params.userLength
      , toData params.adminLength
      , toData params.bondingLength
      , toData params.interestLength
      , toData params.increments
      , toData params.interest
      , toData params.minStake
      , toData params.maxStake
      , toData params.admin
      , toData params.unbondedAssetClass
      , toData params.nftCs
      , toData params.assocListCs
      ]

data UnbondedStakingDatum
  = StateDatum { maybeEntryName :: Maybe ByteArray, isOpen :: Natural } -- isOpen :: Boolean }
  | EntryDatum { entry :: Entry }
  | AssetDatum

derive instance Generic UnbondedStakingDatum _
derive instance Eq UnbondedStakingDatum
instance HasConstrIndices UnbondedStakingDatum where
  constrIndices = defaultConstrIndices

instance ToData UnbondedStakingDatum where
  toData = genericToData

instance Show UnbondedStakingDatum where
  show = genericShow

data UnbondedStakingAction
  = AdminAct { totalRewards :: Natural, totalDeposited :: Natural }
  | StakeAct
      { stakeAmount :: Natural
      , stakeHolder :: PaymentPubKeyHash
      }
  | WithdrawAct { stakeHolder :: PaymentPubKeyHash }
  | CloseAct

derive instance Generic UnbondedStakingAction _
derive instance Eq UnbondedStakingAction
instance HasConstrIndices UnbondedStakingAction where
  constrIndices = defaultConstrIndices

instance ToData UnbondedStakingAction where
  toData = genericToData

newtype Entry =
  Entry
    { key :: ByteArray
    , deposited :: BigInt
    , newDeposit :: BigInt
    , rewards :: Rational
    , totalRewards :: BigInt
    , totalDeposited :: BigInt
    , open :: Boolean
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
