module Types
  ( BondedPoolParams(..)
  , BondedStakingDatum(..)
  , BondedStakingAction(..)
  , AssetClass(..)
  , Entry(..)
  )
  where

import ConstrIndices
import Contract.Numeric.Natural
import Contract.PlutusData
import Data.Ratio
import Data.BigInt(BigInt)
import Data.Tuple
import Prelude
import Types.Interval

import Data.Generic.Rep as G
import Data.Maybe (Maybe)
import ToData (genericToData)
import Types.ByteArray (ByteArray(..))
import Types.NatRatio (NatRatio)
import Types.UnbalancedTransaction (PubKeyHash(..))
import Types.Value (CurrencySymbol, TokenName)

newtype AssetClass = AssetClass ByteArray
derive instance G.Generic AssetClass _
derive instance Eq AssetClass
instance HasConstrIndices AssetClass where
    constrIndices = defaultConstrIndices
instance ToData AssetClass where
    toData = genericToData

-- TODO: Add missing `ToData` instances for POSIXTime and NatRatio.
newtype BondedPoolParams =
    BondedPoolParams {
       iterations :: Natural 
       , start :: BigInt
       , end :: BigInt
       , userLength :: BigInt
       , bondingLength :: BigInt
       , interest :: Ratio Natural
       , minStake :: Natural
       , maxStake :: Natural
       , admin :: PubKeyHash
       , bondedAssetClass :: AssetClass
       , nftCs :: CurrencySymbol
       , assocListCs :: CurrencySymbol
    }
derive instance G.Generic BondedPoolParams _
derive instance Eq BondedPoolParams
instance HasConstrIndices BondedPoolParams where
    constrIndices = defaultConstrIndices
instance ToData BondedPoolParams where
    toData = genericToData

data BondedStakingDatum =
    StateDatum { maybeEntryName :: Maybe ByteArray }
    | EntryDatum { entry :: Entry }
    | AssetDatum
derive instance G.Generic BondedStakingDatum _
derive instance Eq BondedStakingDatum

    
data BondedStakingAction
    = AdminAct { sizeLeft :: Natural }
    | StakeAct { stakeAmount :: Natural
                 , stakeHolder :: PubKeyHash
               }
    | PWithdrawAct { stakeHolder :: PubKeyHash }
    | PCloseAct
derive instance G.Generic BondedStakingAction _
derive instance Eq BondedStakingAction 

newtype Entry =
    Entry {
        key :: ByteArray
        , sizeLeft :: Natural
        , newDeposit :: Natural
        , deposited :: Natural
        , staked :: Natural
        , rewards :: NatRatio
        , value :: Tuple Natural NatRatio
        , next :: Maybe ByteArray
    }
derive instance G.Generic Entry _
derive instance Eq Entry