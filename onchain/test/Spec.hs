module Main (main) where

import Test.SpecNFT (nftTests)
import Test.SpecPeriod (specPeriodTests)
import Prelude (IO)
import Test.Tasty (TestTree, defaultMain, testGroup)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

-- | @since 0.1
main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain tests

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "SingularityNet"
    [
      --nftTests
      specPeriodTests
    ]
