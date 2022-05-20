{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module SingularityNet.Types (
  BondedPoolParams (..),
  BondedStakingAction (..),
  MintingAction (Stake, Withdraw),
  BondedStakingDatum (..),
  Entry (
    Entry,
    key,
    value,
    next,
    sizeLeft,
    newDeposit,
    deposited,
    staked,
    rewards
  ),
  AssetClass (..),
) where

{-
  This module contains all the Haskell-level types used throughout the project.

  Since this needs to be shared in the off-chain and on-chain side, *no*
  Plutarch or fancy GHC9 features are allowed. The Plutarch synonyms for these
  types are in `PTypes`
-}

import GHC.Generics qualified as GHC
import Generics.SOP (Generic)

import PlutusTx (unstableMakeIsData)
import PlutusTx.Builtins.Internal (BuiltinByteString)

import SingularityNet.Natural (
  NatRatio,
  Natural,
 )

import Plutus.V1.Ledger.Api (CurrencySymbol, POSIXTime, PubKeyHash, TokenName)

{- | An `AssetClass` is simply a convenient type that wraps a CurrencySymbol
 and TokenName
-}
data AssetClass = AssetClass
  { acCurrencySymbol :: CurrencySymbol
  , acTokenName :: TokenName
  }
  deriving stock (GHC.Generic, Show)
  deriving anyclass (Generic)

unstableMakeIsData ''AssetClass

{- | Bonded pool's parameters

     These parametrise the staking pool contract. However, the one parameter
     that makes each contract truly unique is `nftCs` (the NFT's
     CurrencySymbol).

     The currency symbol of the associated list (`assocListCs`) is also uniquely
     associated to the `nftCs`.
-}
data BondedPoolParams = BondedPoolParams
  { iterations :: Natural
  , start :: POSIXTime
  , end :: POSIXTime
  , userLength :: POSIXTime
  , bondingLength :: POSIXTime
  , interest :: NatRatio
  , minStake :: Natural
  , maxStake :: Natural
  , admin :: PubKeyHash
  , bondedAssetClass :: AssetClass
  , nftCs :: CurrencySymbol
  , assocListCs :: CurrencySymbol
  }
  deriving stock (GHC.Generic, Show)

unstableMakeIsData ''BondedPoolParams

{- | Associacion list's entry

     An entry in the association list. It keeps track of how much a user staked
     and the pending rewards. It also has a reference to the next entry in the
     list, which might be empty if it is the final element.
-}
data Entry = Entry
  { key :: BuiltinByteString
  , sizeLeft :: Natural
  , newDeposit :: Natural
  , deposited :: Natural
  , staked :: Natural
  , rewards :: NatRatio
  , value :: (Natural, NatRatio)
  , next :: Maybe BuiltinByteString
  }
  deriving stock (Show)

unstableMakeIsData ''Entry

{- | Bonded pool's state

     It can either contain:

     1. A reference to the on-chain association list of stakees-stakes (in the
     case of the pool UTXO)

     2. An entry in the association list (created by the stakers when using
     the StakeAct redeemer)

     3. A dummy datum (in the case of the stake UTXOs)
-}
data BondedStakingDatum
  = StateDatum (Maybe BuiltinByteString) Natural
  | EntryDatum Entry
  | AssetDatum
  deriving stock (Show, GHC.Generic)

unstableMakeIsData ''BondedStakingDatum

{- | Minting redeemers

     These are used for staking and withdrawing funds but they are *not* used
     for consuming the bonded pool's contract, but rather for minting the NFTs
     that comprise each entry in the association list.
-}
data MintingAction
  = Stake
  | Withdraw
  deriving stock (GHC.Generic)

unstableMakeIsData ''MintingAction

{- | Validator redeemers

     These are used by the admin to deposit the rewards and close the pool and
     withdraw the rewards unclaimed.

     These are used by the stakers to deposit their *initial* stake (after that
     they only update their respective entry) and withdrawing their rewards.
-}
data BondedStakingAction
  = AdminAct Natural
  | StakeAct Natural PubKeyHash
  | WithdrawAct PubKeyHash
  | CloseAct
  deriving stock (Show)

unstableMakeIsData ''BondedStakingAction
