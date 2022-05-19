{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
-- This module contains some orphan instances for common Plutarch datatypes. For
-- some reason, these are not available upstream.
{-# OPTIONS_GHC -Wno-orphans #-}

module UnbondedStaking.Types (
  UnbondedPoolParams (..),
  PUnbondedPoolParams,
  UnbondedStakingAction (..),
  PUnbondedStakingAction (..),
  UnbondedStakingDatum (..),
  PUnbondedStakingDatum (..),
  Entry (..),
  PEntry,
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Plutarch.Api.V1 (PMaybeData, PPOSIXTime)
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.TryFrom (PTryFrom)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
 )
import PlutusTx (unstableMakeIsData)
import PlutusTx.Builtins.Internal (BuiltinByteString)

import SingularityNet.Natural (
  NatRatio,
  Natural,
 )

import PNatural (
  PNatRatio,
  PNatural,
 )

import PTypes (PAssetClass)
import SingularityNet.Types (AssetClass)

-- Orphan instance for `PBool`
deriving via
  DerivePNewtype (PAsData PBool) (PAsData PByteString)
  instance
    PTryFrom PData (PAsData PBool)

{- | Unbonded pool's parameters

     These parametrise the staking pool contract. However, the one parameter
     that makes each contract truly unique is `nftCs` (the NFT's
     CurrencySymbol).

     The currency symbol of the associated list (`assocListCs`) is also uniquely
     associated to the `nftCs`.
-}
newtype PUnbondedPoolParams (s :: S)
  = PUnbondedPoolParams
      ( Term
          s
          ( PDataRecord
              '[ "start" ':= PPOSIXTime
               , "userLength" ':= PPOSIXTime
               , "adminLength" ':= PPOSIXTime
               , "bondingLength" ':= PPOSIXTime
               , "interestLength" ':= PPOSIXTime
               , "increments" ':= PNatural
               , "interest" ':= PNatRatio
               , "minStake" ':= PNatural
               , "maxStake" ':= PNatural
               , "admin" ':= PPubKeyHash
               , "unbondedAssetClass" ':= PAssetClass
               , "nftCs" ':= PCurrencySymbol
               , "assocListCs" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PUnbondedPoolParams

deriving via
  PAsData (PIsDataReprInstances PUnbondedPoolParams)
  instance
    PTryFrom PData (PAsData PUnbondedPoolParams)

data UnbondedPoolParams = UnbondedPoolParams
  { start :: POSIXTime -- absolute time
  , userLength :: POSIXTime -- a time delta
  , adminLength :: POSIXTime -- a time delta
  , bondingLength :: POSIXTime -- a time delta
  , interestLength :: POSIXTime -- a time delta
  , increments :: Natural
  , interest :: NatRatio -- interest per increment
  , minStake :: Natural
  , maxStake :: Natural
  , admin :: PubKeyHash
  , unbondedAssetClass :: AssetClass
  , nftCs :: CurrencySymbol -- this uniquely parametrizes the validator
  , assocListCs :: CurrencySymbol -- CurrencySymbol for on-chain associated list UTXOs
  }
  deriving stock (GHC.Generic, Show)

unstableMakeIsData ''UnbondedPoolParams

deriving via
  (DerivePConstantViaData UnbondedPoolParams PUnbondedPoolParams)
  instance
    (PConstant UnbondedPoolParams)

instance PUnsafeLiftDecl PUnbondedPoolParams where
  type PLifted PUnbondedPoolParams = UnbondedPoolParams

{- | Associacion list's entry

     An entry in the association list. It keeps track of how much a user staked
     and the pending rewards. It also has a reference to the next entry in the
     list, which might be empty if it is the final element.
-}
data PEntry (s :: S)
  = PEntry
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PByteString
               , "deposited" ':= PNatural
               , "newDeposit" ':= PNatural
               , "rewards" ':= PNatRatio
               , "totalRewards" ':= PNatural
               , "totalDeposited" ':= PNatural
               , "open" ':= PBool
               , "next" ':= PMaybeData PByteString
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PEntry

deriving via
  PAsData (PIsDataReprInstances PEntry)
  instance
    PTryFrom PData (PAsData PEntry)

data Entry = Entry
  { key :: BuiltinByteString
  , deposited :: Natural
  , newDeposit :: Natural
  , rewards :: NatRatio
  , totalRewards :: Natural
  , totalDeposited :: Natural
  , open :: Bool
  , next :: Maybe BuiltinByteString
  }
  deriving stock (Show)

unstableMakeIsData ''Entry

deriving via
  (DerivePConstantViaData Entry PEntry)
  instance
    (PConstant Entry)

instance PUnsafeLiftDecl PEntry where
  type PLifted PEntry = Entry

{- | Unbonded pool's state

     It can either contain:

     1. A reference to the on-chain association list of stakees-stakes (in the
     case of the pool UTXO)

     2. An entry in the association list (created by the stakers when using
     the StakeAct redeemer)

     3. A dummy datum (in the case of the stake UTXOs)
-}
data PUnbondedStakingDatum (s :: S)
  = PStateDatum
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PMaybeData PByteString
               , "_1" ':= PBool
               ]
          )
      )
  | PEntryDatum
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PEntry
               ]
          )
      )
  | PAssetDatum (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PUnbondedStakingDatum

deriving via
  PAsData (PIsDataReprInstances PUnbondedStakingDatum)
  instance
    (PTryFrom PData (PAsData PUnbondedStakingDatum))

data UnbondedStakingDatum
  = StateDatum (Maybe BuiltinByteString) Bool
  | EntryDatum Entry
  | AssetDatum
  deriving stock (Show, GHC.Generic)

unstableMakeIsData ''UnbondedStakingDatum

deriving via
  (DerivePConstantViaData UnbondedStakingDatum PUnbondedStakingDatum)
  instance
    (PConstant UnbondedStakingDatum)

instance PUnsafeLiftDecl PUnbondedStakingDatum where
  type PLifted PUnbondedStakingDatum = UnbondedStakingDatum

{- | Validator redeemers

     These are used by the admin to deposit the rewards and close the pool and
     withdraw the rewards unclaimed.

     These are used by the stakers to deposit their *initial* stake (after that
     they only update their respective entry) and withdrawing their rewards.
-}
data PUnbondedStakingAction (s :: S)
  = PAdminAct
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PNatural
               , "_1" ':= PNatural
               ]
          )
      )
  | PStakeAct
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PNatural
               , "_1" ':= PPubKeyHash
               ]
          )
      )
  | PWithdrawAct (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PCloseAct (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PUnbondedStakingAction

deriving via
  PAsData (PIsDataReprInstances PUnbondedStakingAction)
  instance
    PTryFrom PData (PAsData PUnbondedStakingAction)

data UnbondedStakingAction
  = AdminAct Natural Natural
  | StakeAct Natural PubKeyHash
  | WithdrawAct PubKeyHash
  | CloseAct
  deriving stock (Show)

unstableMakeIsData ''UnbondedStakingAction

deriving via
  (DerivePConstantViaData UnbondedStakingAction PUnbondedStakingAction)
  instance
    (PConstant UnbondedStakingAction)

instance PUnsafeLiftDecl PUnbondedStakingAction where
  type PLifted PUnbondedStakingAction = UnbondedStakingAction
