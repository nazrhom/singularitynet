module Main (main) where

import Cardano.Prelude (Text)
import Plutus.Contract (Contract)
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Test.Plutip.Contract (initAda, assertExecution, withContract)
import Test.Plutip.Predicate(shouldSucceed)
import Test.Plutip.LocalCluster (withCluster)
import Test.Tasty (TestTree, defaultMain, testGroup)

import SingularityNetOffchain(loadPlutusScript)
import SpecStateNFT(specStateNFT)

main :: IO ()
main = do
  Just statePolicyScript <- loadPlutusScript "BondedStateNFT"
  defaultMain $ testGroup
    "SingularityNet off-chain tests"
    [
      specStateNFT statePolicyScript 
    ]
