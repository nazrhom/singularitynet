module SingularityNet.Test (main) where

import Contract.Prelude

import Contract.Monad (defaultContractConfig, runContract_)
import Effect.Aff (launchAff_)
import SingularityNet.Contract.MintNft (mintBondedStateNft)

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultContractConfig
  runContract_ cfg mintBondedStateNft

