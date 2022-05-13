module UnbondedStaking.Types
  ( UnbondedPoolParams(..)
  , UnbondedStakingDatum(..)
  , UnbondedStakingAction(..)
  , Entry(..)
  , StakingType
  ) where

import Contract.Prelude

import ConstrIndices (class HasConstrIndices, defaultConstrIndices)
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Rational (Rational)
import Contract.PlutusData (class ToData, PlutusData(..), toData)
import Contract.Value (CurrencySymbol)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import ToData (genericToData)
import Types (AssetClass)
import Types.ByteArray (ByteArray)
import Types.UnbalancedTransaction (PaymentPubKeyHash)
import Utils (big)

-- TODO: Add missing `ToData` instances for POSIXTime and NatRatio.
newtype UnbondedPoolParams =
  UnbondedPoolParams
    { upp'start :: BigInt
    , upp'userLength :: BigInt
    , upp'adminLength :: BigInt
    , upp'bondingLength :: BigInt
    , upp'interestLength :: BigInt
    , upp'increments :: Natural
    , upp'interest :: Rational
    , upp'minStake :: Natural
    , upp'maxStake :: Natural
    , upp'admin :: PaymentPubKeyHash
    , upp'unbondedAssetClass :: AssetClass
    , upp'nftCs :: CurrencySymbol
    , upp'assocListCs :: CurrencySymbol
    }

derive instance Generic UnbondedPoolParams _
derive instance Eq UnbondedPoolParams
derive instance Newtype UnbondedPoolParams _
instance HasConstrIndices UnbondedPoolParams where
  constrIndices = defaultConstrIndices

-- We copy the order of the fields from the Haskell implementation
instance ToData UnbondedPoolParams where
  toData (UnbondedPoolParams params) =
    Constr (big 0)
      [ toData params.upp'start
      , toData params.upp'userLength
      , toData params.upp'adminLength
      , toData params.upp'bondingLength
      , toData params.upp'interestLength
      , toData params.upp'increments
      , toData params.upp'interest
      , toData params.upp'minStake
      , toData params.upp'maxStake
      , toData params.upp'admin
      , toData params.upp'unbondedAssetClass
      , toData params.upp'nftCs
      , toData params.upp'assocListCs
      ]

data UnbondedStakingDatum
  = StateDatum { maybeEntryName :: Maybe ByteArray, isOpen :: Boolean }
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
