module BondedPool (
  pbondedPoolValidator,
  hbondedPoolValidator,
  bondedPoolValidatorHash,
) where

import Plutarch (compile)
import Plutarch.Api.V1 (scriptHash)
import Plutus.V1.Ledger.Api (Script)
import Plutus.V1.Ledger.Scripts (ScriptHash)
import Types (
  BondedPoolParams,
  PBondedPoolParams,
  PBondedStakingAction,
  PBondedStakingDatum,
 )

pbondedPoolValidator ::
  forall (s :: S).
  Term
    s
    ( PBondedPoolParams
        :--> PAsData PBondedStakingDatum
        :--> PAsData PBondedStakingAction
        :--> PUnit
    )
pbondedPoolValidator = phoistAcyclic $ plam $ \_ _ _ -> pconstant ()

hbondedPoolValidator :: BondedPoolParams -> Script
hbondedPoolValidator bpp = compile $ pbondedPoolValidator # pconstant bpp

bondedPoolValidatorHash :: BondedPoolParams -> ScriptHash
bondedPoolValidatorHash = scriptHash . hbondedPoolValidator
