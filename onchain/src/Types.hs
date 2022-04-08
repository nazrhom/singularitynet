module Types (BondedStakingState(BondedStakingState)) where

import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.AssocMap (PMap)

newtype BondedStakingState (s :: S) =
  BondedStakingState (Term s (PMap PPubKeyHash PInteger))
  deriving PIsData
    via (DerivePNewtype BondedStakingState (PMap PPubKeyHash PInteger))