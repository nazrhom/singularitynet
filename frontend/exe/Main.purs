module Main (main) where

import Contract.Prelude

import Contract.Monad (defaultContractConfig, runContract_)
import CreatePool (createPoolContract)
import DepositPool (depositPoolContract)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ $ do
  cfg <- defaultContractConfig
  runContract_ cfg createPoolContract
  --runContract_ cfg depositPoolContract
