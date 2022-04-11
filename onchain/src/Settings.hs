module Settings (bondedStakingTokenName) where

{-
    This module is for hardcoded platform settings
-}

import Plutus.V1.Ledger.Api (TokenName)

bondedStakingTokenName :: TokenName
bondedStakingTokenName = "BondedStakingToken"
