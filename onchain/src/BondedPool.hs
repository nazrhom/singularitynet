module BondedPool (
  pbondedPoolValidator,
  hbondedPoolValidator,
) where

import Plutarch.Api.V1 (mkValidator)
import Plutus.V1.Ledger.Api (Validator)
import Types (
  BondedPoolParams,
  PBondedPoolParams,
  PBondedStakingAction,
  PBondedStakingState,
  -- PBondedStakingDatum,
 )
import Plutarch.Unsafe (punsafeCoerce)

pbondedPoolValidator ::
  forall (s :: S).
  Term
    s
    ( PBondedPoolParams
        :--> PAsData PBondedStakingState
        :--> PAsData PBondedStakingAction
        :--> PUnit
    )
pbondedPoolValidator = phoistAcyclic $ plam $ \_ _ _ -> pconstant ()

hbondedPoolValidator :: BondedPoolParams -> Validator
hbondedPoolValidator params =
  mkValidator $ punsafeCoerce $ pbondedPoolValidator # pconstant params