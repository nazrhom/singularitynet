{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Types (
  BondedStakingState (BondedStakingState),
  PBondedStakingState (PBondedStakingState),
  BondedPoolParams (BondedPoolParams),
  PBondedPoolParams (PBondedPoolParams),
  BondedStakingAction,
  PBondedStakingAction,
  BondedStakingDatum (BondedStakingStateDatum, BondedAssetDatum),
  PBondedStakingDatum (PBondedStakingStateDatum, PBondedAssetDatum),
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

-- import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  PubKeyHash (PubKeyHash),
 )
import PlutusTx (unstableMakeIsData)
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))

-- newtype BondedStakingState (s :: S)
--  = BondedStakingState (Term s (PMap PPubKeyHash PInteger))
--  deriving
--    (PIsData)
--    via (DerivePNewtype BondedStakingState (PMap PPubKeyHash PInteger))

{- | This is the pool's state, a map from each stakee to its stake
 amount
-}
newtype PBondedStakingState (s :: S)
  = PBondedStakingState (Term s (PBuiltinList (PAsData PPubKeyHash)))
  deriving
    (PlutusType, PIsData)
    via (DerivePNewtype PBondedStakingState (PBuiltinList (PAsData PPubKeyHash)))

instance PUnsafeLiftDecl PBondedStakingState where
  type PLifted PBondedStakingState = BondedStakingState

newtype BondedStakingState = BondedStakingState [PubKeyHash]
  deriving stock (Show)
  deriving
    (PConstant)
    via DerivePConstantViaNewtype
          BondedStakingState
          PBondedStakingState
          (PBuiltinList PByteString)

unstableMakeIsData ''BondedStakingState

{- | These parametrize the staking pool contract. However, the one parameter
 that makes each contract truly unique is `BondedStakingStateCs` (the NFT
 hash)
-}
newtype PBondedPoolParams (s :: S)
  = PBondedPoolParams
      ( Term
          s
          ( PDataRecord
              '[ "operator" ':= PPubKeyHash
               , "bondedStakingStateCs" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PBondedPoolParams

data BondedPoolParams = BondedPoolParams
  { operator :: PubKeyHash
  , bondedStakingStateCs :: CurrencySymbol
  }
  deriving stock (GHC.Generic, Show)

unstableMakeIsData ''BondedPoolParams

deriving via
  (DerivePConstantViaData BondedPoolParams PBondedPoolParams)
  instance
    (PConstant BondedPoolParams)

instance PUnsafeLiftDecl PBondedPoolParams where
  type PLifted PBondedPoolParams = BondedPoolParams

{- | The state associated with the bonded pool contract. It can either contain
 the map of stakees to quantities staked (in the case of the pool UTXO), or
 a dummy datum (in the case of the stake UTXOs)
-}
data PBondedStakingDatum (s :: S)
  = PBondedStakingStateDatum
      (Term s (PDataRecord '["_0" ':= PBondedStakingState]))
  | PBondedAssetDatum (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PBondedStakingDatum

data BondedStakingDatum
  = BondedStakingStateDatum BondedStakingState
  | BondedAssetDatum
  deriving stock (GHC.Generic, Show)

unstableMakeIsData ''BondedStakingDatum

deriving via
  (DerivePConstantViaData BondedStakingDatum PBondedStakingDatum)
  instance
    (PConstant BondedStakingDatum)

instance PUnsafeLiftDecl PBondedStakingDatum where
  type PLifted PBondedStakingDatum = BondedStakingDatum

{- | The possible redeemers the bonded pool contract may accept. Each is
 associated to an endpoint.
-}
type BondedStakingAction = ()

type PBondedStakingAction = PUnit
