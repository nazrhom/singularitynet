module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import SingularityNetOffchain (loadPlutusScript)

import Types (BondedPoolScripts (BondedPoolScripts, listPolicyScript, statePolicyScript, validatorScript))

import SpecStateNFT (specStateNFT)

main :: IO ()
main = do
  Just statePolicyScript <- loadPlutusScript "BondedStateNFT"
  Just listPolicyScript <- loadPlutusScript "BondedListNFT"
  Just validatorScript <- loadPlutusScript "BondedPoolValidator"
  let bondedPoolScripts = BondedPoolScripts {statePolicyScript, listPolicyScript, validatorScript}
  defaultMain $
    testGroup
      "SingularityNet off-chain tests"
      [ specStateNFT statePolicyScript
      ]
