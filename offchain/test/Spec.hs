module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import SingularityNetOffchain (loadPlutusScript)

import SpecStateNFT (specStateNFT)

main :: IO ()
main = do
  Just statePolicyScript <- loadPlutusScript "BondedStateNFT"
  defaultMain $
    testGroup
      "SingularityNet off-chain tests"
      [ specStateNFT statePolicyScript
      ]
