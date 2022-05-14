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

module PTypes (
  PBondedPoolParams (..),
  PBondedStakingAction (..),
  PMintingAction,
  PBondedStakingDatum (..),
  PEntry,
  PAssetClass (PAssetClass),
  passetClass,
) where

{-
 This module contains all the Plutarch-level synonyms of the types defined in
 `Types`
-}

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Common.Types (AssetClass, BondedPoolParams, BondedStakingAction, BondedStakingDatum, Entry, MintingAction)
import PNatural (PNatRatio, PNatural)

import Plutarch.Api.V1 (PMaybeData, PPOSIXTime, PTokenName, PTxId, PTxOutRef)
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

----- Plutarch synonyms -----

-- | `AssetClass` synonym
newtype PAssetClass (s :: S)
  = PAssetClass
      ( Term
          s
          ( PDataRecord
              '[ "currencySymbol" ':= PCurrencySymbol
               , "tokenName" ':= PTokenName
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PAssetClass

deriving via
  PAsData (PIsDataReprInstances PAssetClass)
  instance
    PTryFrom PData (PAsData PAssetClass)

instance PUnsafeLiftDecl PAssetClass where
  type PLifted PAssetClass = AssetClass

-- | Builds a `PAssetClass`
passetClass ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PTokenName :--> PAssetClass)
passetClass = phoistAcyclic $
  plam $ \cs tn ->
    pcon $
      PAssetClass $
        pdcons # pdata cs
          #$ pdcons # pdata tn # pdnil

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
               , "sizeLeft" ':= PNatural
               , "newDeposit" ':= PNatural
               , "deposited" ':= PNatural
               , "staked" ':= PNatural
               , "rewards" ':= PNatRatio
               , "value" ':= PBuiltinPair (PAsData PNatural) (PAsData PNatRatio)
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
               , "_1" ':= PNatural
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

-- | `MintingAction` synonym
data PMintingAction (s :: S)
  = PStake (Term s (PDataRecord '[]))
  | PWithdraw (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PMintingAction

deriving via
  PAsData (PIsDataReprInstances PMintingAction)
  instance
    PTryFrom PData (PAsData PMintingAction)

instance PUnsafeLiftDecl PMintingAction where
  type PLifted PMintingAction = MintingAction

-- | `BondedStakingAction` synonym
data PBondedStakingAction (s :: S)
  = PAdminAct (Term s (PDataRecord '["_0" ':= PNatural]))
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
    via PIsDataReprInstances PBondedStakingAction

deriving via
  PAsData (PIsDataReprInstances PBondedStakingAction)
  instance
    PTryFrom PData (PAsData PBondedStakingAction)

instance PUnsafeLiftDecl PBondedStakingAction where
  type PLifted PBondedStakingAction = BondedStakingAction

------ Orphans ------

---- PTryFrom instances ----
-- Orphan instance for `PMaybeData PByteString`
deriving via
  PAsData (PIsDataReprInstances (PMaybeData PByteString))
  instance
    PTryFrom PData (PAsData (PMaybeData PByteString))

-- Orphan instance for `PTxOutRef`
deriving via
  PAsData (PIsDataReprInstances PTxOutRef)
  instance
    PTryFrom PData (PAsData PTxOutRef)

-- Orphan instance for `PTxId`
deriving via
  PAsData (PIsDataReprInstances PTxId)
  instance
    PTryFrom PData (PAsData PTxId)

-- Orphan instance for `PCurrencySymbol`
deriving via
  DerivePNewtype (PAsData PCurrencySymbol) (PAsData PByteString)
  instance
    PTryFrom PData (PAsData PCurrencySymbol)

-- Orphan instance for `PPubKeyHash`
deriving via
  DerivePNewtype (PAsData PPubKeyHash) (PAsData PByteString)
  instance
    PTryFrom PData (PAsData PPubKeyHash)

-- Orphan instance for `PPOSIXTime`
deriving via
  DerivePNewtype (PAsData PPOSIXTime) (PAsData PInteger)
  instance
    PTryFrom PData (PAsData PPOSIXTime)

-- Orphan instance for `PTokenName`
deriving via
  DerivePNewtype (PAsData PTokenName) (PAsData PByteString)
  instance
    PTryFrom PData (PAsData PTokenName)

---- PConstant instances ----

deriving via
  ( DerivePConstantViaData
      AssetClass
      PAssetClass
  )
  instance
    PConstant AssetClass

deriving via
  (DerivePConstantViaData BondedPoolParams PBondedPoolParams)
  instance
    (PConstant BondedPoolParams)

deriving via
  (DerivePConstantViaData Entry PEntry)
  instance
    (PConstant Entry)

deriving via
  (DerivePConstantViaData MintingAction PMintingAction)
  instance
    (PConstant MintingAction)

deriving via
  (DerivePConstantViaData BondedStakingAction PBondedStakingAction)
  instance
    (PConstant BondedStakingAction)
