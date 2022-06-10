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
  HField,
  PAssetClass (PAssetClass),
  PMintingAction (..),
  PBurningAction (..),
  PListAction (..),
  PTxInfoFields,
  PTxInfoHRec,
  PTxInInfoFields,
  PTxInInfoHRec,
  PPeriod (..),
  passetClass,
  unavailablePeriod,
  depositWithdrawPeriod,
  adminUpdatePeriod,
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
import GHC.TypeLits (Symbol)
import Generics.SOP (Generic, I (I))

import SingularityNet.Types (
  AssetClass,
  BurningAction,
  ListAction,
  MintingAction,
 )

import Plutarch.Api.V1 (
  PDCert,
  PDatum,
  PDatumHash,
  PMaybeData,
  PPOSIXTime,
  PPOSIXTimeRange,
  PStakingCredential,
  PTokenName,
  PTuple,
  PTxId,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
  PValue,
 )
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  HRec,
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.DataRepr.Internal.Field (Labeled)
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
  = PBurnHead
      ( Term
          s
          ( PDataRecord
              '[ "state" ':= PTxOutRef
               , "headEntry" ':= PTxOutRef
               ]
          )
      )
  | PBurnOther
      ( Term
          s
          ( PDataRecord
              '[ "previousEntry" ':= PTxOutRef
               , "burnEntry" ':= PTxOutRef
               ]
          )
      )
  | PBurnSingle (Term s (PDataRecord '["burnEntry" ':= PTxOutRef]))
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

{- | A newtype used internally for encoding different periods.

   Depending on the pool's parameters, a certain period can either be:

   0. UnavailablePeriod: The pool has not started yet and no actions are
      permitted.
   1. DepositWithdrawPeriod: A user can both stake and deposit
   2. AdminUpdatePeriod: (Unbonded) - Only admin actions are allowed
   3. BondingPeriod:
        * Bonded: Only admin actions are allowed
        * Unbonded: Users can only withdraw
   4. OnlyWithdrawPeriod: Users can only withdraw, this happens once in the
      lifetime of a pool, before closing.
   5. ClosingPeriod: The admin can withdraw the remaining funds and close the
      pool
-}
data PPeriod (s :: S)
  = UnavailablePeriod
  | DepositWithdrawPeriod
  | AdminUpdatePeriod
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

adminUpdatePeriod :: forall (s :: S). Term s PPeriod
adminUpdatePeriod = pcon AdminUpdatePeriod

bondingPeriod :: forall (s :: S). Term s PPeriod
bondingPeriod = pcon BondingPeriod

onlyWithdrawPeriod :: forall (s :: S). Term s PPeriod
onlyWithdrawPeriod = pcon OnlyWithdrawPeriod

closingPeriod :: forall (s :: S). Term s PPeriod
closingPeriod = pcon ClosingPeriod

----- Type synonyms -----

-- Useful type family for reducing boilerplate in HRec types
type family HField (s :: S) (field :: Symbol) (ptype :: PType) where
  HField s field ptype = Labeled field (Term s (PAsData ptype))

-- | HRec with all of `PTxInfo`'s fields
type PTxInfoHRec (s :: S) =
  HRec
    '[ HField s "inputs" (PBuiltinList (PAsData PTxInInfo))
     , HField s "outputs" (PBuiltinList (PAsData PTxOut))
     , HField s "fee" PValue
     , HField s "mint" PValue
     , HField s "dcert" (PBuiltinList (PAsData PDCert))
     , HField s "wdrl" (PBuiltinList (PAsData (PTuple PStakingCredential PInteger)))
     , HField s "validRange" PPOSIXTimeRange
     , HField s "signatories" (PBuiltinList (PAsData PPubKeyHash))
     , HField s "data" (PBuiltinList (PAsData (PTuple PDatumHash PDatum)))
     , HField s "id" PTxId
     ]

-- | HRec with all of `PTxInInfo`'s fields
type PTxInInfoHRec (s :: S) =
  HRec
    '[ HField s "outRef" PTxOutRef
     , HField s "resolved" PTxOut
     ]

-- | Type level list with all of `PTxInfo`'s fields
type PTxInfoFields =
  '[ "inputs"
   , "outputs"
   , "fee"
   , "mint"
   , "dcert"
   , "wdrl"
   , "validRange"
   , "signatories"
   , "data"
   , "id"
   ]

-- | Type level list with all of `PTxInInfo`'s fields
type PTxInInfoFields =
  '[ "outRef"
   , "resolved"
   ]

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
