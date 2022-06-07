{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module SingularityNet.Types (
  BondedPoolParams (..),
  MintingAction (..),
  BurningAction (..),
  ListAction (..),
  BondedStakingAction (..),
  BondedStakingDatum (..),
  Entry (
    Entry,
    key,
    next,
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

import Plutus.V1.Ledger.Api (CurrencySymbol, POSIXTime, PubKeyHash, TokenName, TxOutRef)

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
  , newDeposit :: Natural
  , deposited :: Natural
  , staked :: Natural
  , rewards :: NatRatio
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
  = StateDatum (Maybe BuiltinByteString)
  | EntryDatum Entry
  | AssetDatum
  deriving stock (Show, GHC.Generic)

unstableMakeIsData ''BondedStakingDatum

{- | Minting / Burning actions

     These datatypes are used to describe to the minting policy or validator
     where an entry will be inserted or which entry will be removed. These
     aid the policy and validator by restricting the actions to be checked to
     only one.
     
     In the minting policy, these are used as redeemers. In the validator, these
     are wrapped in a `Maybe` and are used only for the first stake of any user
-}
data MintingAction
  = MintHead TxOutRef
  | MintInBetween TxOutRef TxOutRef
  | MintEnd TxOutRef
  deriving stock (GHC.Generic, Show)

unstableMakeIsData ''MintingAction

data BurningAction
  = BurnHead TxOutRef TxOutRef
  | BurnOther TxOutRef TxOutRef
  deriving stock (GHC.Generic, Show)
  
unstableMakeIsData ''BurningAction

-- | Minting policy redeemers
data ListAction
  = ListInsert MintingAction
  | ListRemove BurningAction
  deriving stock (GHC.Generic, Show)
  
unstableMakeIsData ''ListAction

{- | Validator redeemers

     These are used by the admin to deposit the rewards, close the pool and
     withdraw the unclaimed rewards.

     These are used by the stakers to deposit their first stake or update their
     already existing stake.
     
     When the stake-holder makes their first deposit, the redeemer will be
     `StakeAct amt pkh (Just mintAct)`, where `mintAct` specifies the type of
     insertion that needs to checked by the validator.

     When the stake-holder wants to update their existing stake, the last field
     is set to `Nothing`, since no entry needs to be inserted or removed from
     the list.
     
     When the stake-holder wants to claim the their rewards, the redeemer will
     be `WithdrawAct pkh burnAct`, where `burnAct` specifies the type of removal
     that needs to be checked by the validator.
-}
data BondedStakingAction
  = AdminAct
  | StakeAct Natural PubKeyHash (Maybe MintingAction)
  | WithdrawAct PubKeyHash BurningAction
  | CloseAct
  deriving stock (Show)

unstableMakeIsData ''BondedStakingAction
