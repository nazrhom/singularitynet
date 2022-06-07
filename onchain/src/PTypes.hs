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
  PMintingAction (..),
  PBurningAction (..),
  PListAction (..),
  PBondedStakingAction (..),
  PBondedStakingDatum (..),
  PEntry,
  PAssetClass (PAssetClass),
  PPeriod (..),
  passetClass,
  unavailablePeriod,
  depositWithdrawPeriod,
  bondingPeriod,
  onlyWithdrawPeriod,
  closingPeriod,
) where

{-
 This module contains all the Plutarch-level synonyms of the types defined in
 `Types`. There are also other Plutarch types that are only used internally (like
 `PPeriod`)
-}

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import PNatural (PNatRatio, PNatural)
import SingularityNet.Types (
  AssetClass,
  BondedPoolParams,
  BondedStakingAction,
  BondedStakingDatum,
  BurningAction,
  Entry,
  ListAction,
  MintingAction,
 )

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
  = PMintHead (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PMintInBetween
      ( Term
          s
          ( PDataRecord
              '["previousEntry" ':= PTxOutRef, "currentEntry" ':= PTxOutRef]
          )
      )
  | PMintEnd (Term s (PDataRecord '["_0" ':= PTxOutRef]))
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

-- | `BurningAction` synonym
data PBurningAction (s :: S)
  = PBurnHead (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PBurnOther (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PBurningAction

deriving via
  PAsData (PIsDataReprInstances PBurningAction)
  instance
    PTryFrom PData (PAsData PBurningAction)

instance PUnsafeLiftDecl PBurningAction where
  type PLifted PBurningAction = BurningAction

-- | `ListAction` synonym
data PListAction (s :: S)
  = PListInsert (Term s (PDataRecord '["_0" ':= PMintingAction]))
  | PListRemove (Term s (PDataRecord '["_0" ':= PBurningAction]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PListAction

deriving via
  PAsData (PIsDataReprInstances PListAction)
  instance
    PTryFrom PData (PAsData PListAction)

instance PUnsafeLiftDecl PListAction where
  type PLifted PListAction = ListAction

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

{- | A newtype used internally for encoding different periods.

   Depending on the pool's parameters, a certain period can either be:

   0. UnavailablePeriod: The pool has not started yet and no actions are
      permitted.
   1. DepositWithdrawPeriod: A user can both stake and deposit
   2. BondingPeriod: Only admin actions are allowed
   3. OnlyWithdrawPeriod: Users can only withdraw, this happens once in the
      lifetime of a pool, before closing.
   4. ClosingPeriod: The admin can withdraw the remaining funds and close the
      pool
-}
data PPeriod (s :: S)
  = UnavailablePeriod
  | DepositWithdrawPeriod
  | BondingPeriod
  | OnlyWithdrawPeriod
  | ClosingPeriod
  deriving stock (GHC.Generic, Eq)
  deriving anyclass (Generic, PlutusType)

{- | Compares datatypes that don't have a `PEq` instance but do have a `Eq`
 and `PlutusType` instance
-}
plutusEq ::
  forall (s :: S) (a :: PType).
  (PlutusType a, Eq (a s)) =>
  Term s a ->
  Term s a ->
  Term s PBool
plutusEq a' b' = pmatch a' $ \a -> pmatch b' $ \b -> pconstant $ a == b

instance PEq PPeriod where
  (#==) = plutusEq

-- Useful constants
unavailablePeriod :: forall (s :: S). Term s PPeriod
unavailablePeriod = pcon UnavailablePeriod

depositWithdrawPeriod :: forall (s :: S). Term s PPeriod
depositWithdrawPeriod = pcon DepositWithdrawPeriod

bondingPeriod :: forall (s :: S). Term s PPeriod
bondingPeriod = pcon BondingPeriod

onlyWithdrawPeriod :: forall (s :: S). Term s PPeriod
onlyWithdrawPeriod = pcon OnlyWithdrawPeriod

closingPeriod :: forall (s :: S). Term s PPeriod
closingPeriod = pcon ClosingPeriod

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
  (DerivePConstantViaData BurningAction PBurningAction)
  instance
    (PConstant BurningAction)

deriving via
  (DerivePConstantViaData ListAction PListAction)
  instance
    (PConstant ListAction)

deriving via
  (DerivePConstantViaData BondedStakingAction PBondedStakingAction)
  instance
    (PConstant BondedStakingAction)
