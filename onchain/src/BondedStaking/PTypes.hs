{-# LANGUAGE UndecidableInstances #-}
{- This module contains orphan instances. This is because of two reasons:

    * Some Plutarch types don't have `PTryFrom` instances upstream. This might
      be related to the fact that we are not using Plutarch staging, but rather
      the master version with a backport of the TryFrom feature

    * The module `Types` needs to be compiled separately in the off-chain side
      as well, which does not have Plutarch. Because of this, `PConstant`
      instances need to be defined here instead
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BondedStaking.PTypes (
  PBondedPoolParams (..),
  PBondedPoolParamsHRec,
  PBondedPoolParamsFields,
  PBondedStakingAction (..),
  PBondedStakingDatum (..),
  PEntry,
  PEntryFields,
  PEntryHRec,
) where

{-
 This module contains all the Plutarch-level synonyms of the types defined in
 `Types`. There are also other Plutarch types that are only used internally (like
 `PPeriod`)
-}

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import PNatural (PNatRatio, PNatural)
import PTypes (
  HField,
  PAssetClass,
  PBurningAction,
  PMintingAction,
 )
import SingularityNet.Types (
  BondedPoolParams,
  BondedStakingAction,
  BondedStakingDatum,
  Entry,
 )

import Plutarch.Api.V1 (PMaybeData, PPOSIXTime)
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  HRec,
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.TryFrom (PTryFrom)

----- Plutarch synonyms -----

-- | `BondedPoolParams` synonym
newtype PBondedPoolParams (s :: S)
  = PBondedPoolParams
      ( Term
          s
          ( PDataRecord
              '[ "iterations" ':= PNatural
               , "start" ':= PPOSIXTime
               , "end" ':= PPOSIXTime
               , "userLength" ':= PPOSIXTime
               , "bondingLength" ':= PPOSIXTime
               , "interest" ':= PNatRatio
               , "minStake" ':= PNatural
               , "maxStake" ':= PNatural
               , "admin" ':= PPubKeyHash
               , "bondedAssetClass" ':= PAssetClass
               , "nftCs" ':= PCurrencySymbol
               , "assocListCs" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PBondedPoolParams

deriving via
  PAsData (PIsDataReprInstances PBondedPoolParams)
  instance
    PTryFrom PData (PAsData PBondedPoolParams)

instance PUnsafeLiftDecl PBondedPoolParams where
  type PLifted PBondedPoolParams = BondedPoolParams

-- | `Entry` synonym
data PEntry (s :: S)
  = PEntry
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PByteString
               , "newDeposit" ':= PNatural
               , "deposited" ':= PNatural
               , "staked" ':= PNatural
               , "rewards" ':= PNatRatio
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

instance PUnsafeLiftDecl PEntry where
  type PLifted PEntry = Entry

-- | `BondedStakingDatum` synonym
data PBondedStakingDatum (s :: S)
  = PStateDatum
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PMaybeData PByteString
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
    via PIsDataReprInstances PBondedStakingDatum

deriving via
  PAsData (PIsDataReprInstances PBondedStakingDatum)
  instance
    (PTryFrom PData (PAsData PBondedStakingDatum))

deriving via
  (DerivePConstantViaData BondedStakingDatum PBondedStakingDatum)
  instance
    (PConstant BondedStakingDatum)

instance PUnsafeLiftDecl PBondedStakingDatum where
  type PLifted PBondedStakingDatum = BondedStakingDatum

-- | `BondedStakingAction` synonym
data PBondedStakingAction (s :: S)
  = PAdminAct (Term s (PDataRecord '[]))
  | PStakeAct
      ( Term
          s
          ( PDataRecord
              '[ "stakeAmount" ':= PNatural
               , "pubKeyHash" ':= PPubKeyHash
               , "maybeMintingAction" ':= PMaybeData PMintingAction
               ]
          )
      )
  | PWithdrawAct
      ( Term
          s
          ( PDataRecord
              '[ "pubKeyHash" ':= PPubKeyHash
               , "burningAction" ':= PBurningAction
               ]
          )
      )
  | PCloseAct (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PBondedStakingAction

deriving via
  PAsData (PIsDataReprInstances (PMaybeData PMintingAction))
  instance
    PTryFrom PData (PAsData (PMaybeData PMintingAction))

deriving via
  PAsData (PIsDataReprInstances PBondedStakingAction)
  instance
    PTryFrom PData (PAsData PBondedStakingAction)

instance PUnsafeLiftDecl PBondedStakingAction where
  type PLifted PBondedStakingAction = BondedStakingAction

----- Type synonyms -----

-- | HRec with all of `PEntry`'s fields
type PEntryHRec (s :: S) =
  HRec
    '[ HField s "key" PByteString
     , HField s "newDeposit" PNatural
     , HField s "deposited" PNatural
     , HField s "staked" PNatural
     , HField s "rewards" PNatRatio
     , HField s "next" (PMaybeData PByteString)
     ]

-- | Type level list with all of `PEntry`'s fields
type PEntryFields =
  '[ "key"
   , "newDeposit"
   , "deposited"
   , "staked"
   , "rewards"
   , "next"
   ]

-- | HRec with all of `PBondedPoolParams`'s fields
type PBondedPoolParamsHRec (s :: S) =
  HRec
    '[ HField s "iterations" PNatural
     , HField s "start" PPOSIXTime
     , HField s "end" PPOSIXTime
     , HField s "userLength" PPOSIXTime
     , HField s "bondingLength" PPOSIXTime
     , HField s "interest" PNatRatio
     , HField s "minStake" PNatural
     , HField s "maxStake" PNatural
     , HField s "admin" PPubKeyHash
     , HField s "bondedAssetClass" PAssetClass
     , HField s "nftCs" PCurrencySymbol
     , HField s "assocListCs" PCurrencySymbol
     ]

-- | Type level list with all of `PBondedPoolParams's field names
type PBondedPoolParamsFields =
  '[ "iterations"
   , "start"
   , "end"
   , "userLength"
   , "bondingLength"
   , "interest"
   , "minStake"
   , "maxStake"
   , "admin"
   , "bondedAssetClass"
   , "nftCs"
   , "assocListCs"
   ]

---- PConstant instances ----

deriving via
  (DerivePConstantViaData BondedPoolParams PBondedPoolParams)
  instance
    (PConstant BondedPoolParams)

deriving via
  (DerivePConstantViaData Entry PEntry)
  instance
    (PConstant Entry)

deriving via
  (DerivePConstantViaData BondedStakingAction PBondedStakingAction)
  instance
    (PConstant BondedStakingAction)
