module SingularityNet.MintingPolicy
  ( nftMintingPolicy
  ) where

import Contract.Prelude
import Contract.Scripts (MintingPolicy)
import Data.Argonaut (Json, JsonDecodeError)
import SingularityNet.Helpers (jsonReader)

-- This should be the minting policy applied to all parameters, in our case,
-- the TxOutRef. Once we have `applyCode`, we can change this to be unapplied
-- and apply code on Purescript side.
nftMintingPolicy :: Either JsonDecodeError MintingPolicy
nftMintingPolicy = jsonReader "vietsNftMintingPolicy" _mintingPolicy

foreign import _mintingPolicy :: Json