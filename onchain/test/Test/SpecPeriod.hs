module Test.SpecPeriod (
  specPeriodTests,
) where

{-
    module description here
-}

import PTypes(PPeriod(..))
import PInterval (
  PPeriodicInterval(..),
  pperiodicContains,
  pinterval)
import Plutarch.Api.V1 (PPOSIXTime)
import Plutarch.Api.V1.Interval(PInterval)
import Common.Natural (Natural(Natural))

import Test.Tasty
import Test.Tasty.HUnit(testCase)
import Test.Utils (returnsTrue)

specPeriodTests :: TestTree
specPeriodTests =
    testGroup
      "pperiodicContains tests"
      [
        testCase "3000 ∈ 1000 + [1000; 2000) % 5000" $
          returnsTrue $ testWith rangeInside
      ]

testPeriodicInterval :: Term s PPeriodicInterval
testPeriodicInterval = pcon $ PPeriodicInterval {
  piBaseOffset = pconstant 1000
  , piPeriod = pconstant 5000
  , piStartOffset = pconstant 1000
  , piEndOffset = pconstant 2000
  , piMaxCycles = pconstant (Natural 5)
}

testWith :: Term s (PInterval PPOSIXTime) -> Term s PBool
testWith txRange = pperiodicContains # testPeriodicInterval # txRange

-- TX range inside interval in first cycle
rangeInside :: Term s (PInterval PPOSIXTime)
rangeInside = pinterval (pconstant 2200) (pconstant 2800)