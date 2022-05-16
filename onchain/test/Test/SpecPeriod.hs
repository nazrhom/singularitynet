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
import Test.Utils (returnsTrue, returnsFalse)

specPeriodTests :: TestTree
specPeriodTests =
    testGroup
      "pperiodicContains tests"
      [
        testCase "[2200; 2800) ∈ 1000 + [1000; 2000) % 5000" $
          returnsTrue $ testWith rangeInside
        , testCase "[1000; 2000) ∈ 1000 + [1000; 2000) % 5000" $
          returnsTrue $ testWith rangeExact
        , testCase "[12,000; 13,000) ∈ 1000 + 3 * [1000; 2000) % 5000" $
          returnsTrue $ testWith rangeExactOtherCycle
        , testCase "[27,000; 28,000) ∈ 1000 + [1000; 2000) % 5000" $
          returnsFalse $ testWith rangeInsideBadCycle
        , testCase "[1500; 2500) ∈ 1000 + [1000; 2000) % 5000" $
          returnsFalse $ testWith rangeStartsTooSoon
        , testCase "[2500; 3500) ∈ 1000 + [1000; 2000) % 5000" $
          returnsFalse $ testWith rangeEndsTooLate
        , testCase "[2500; 7500) ∈ 1000 + [1000; 2000) % 5000" $
          returnsFalse $ testWith rangeTooWide
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

-- TX range is inside interval in first cycle
rangeInside :: Term s (PInterval PPOSIXTime)
rangeInside = pinterval (pconstant 2200) (pconstant 2800)

-- TX range is *exactly* the interval in first cycle
rangeExact :: Term s (PInterval PPOSIXTime)
rangeExact = pinterval (pconstant 2000) (pconstant 3000)

-- TX range is inside interval in 3rd cycle. It is also exact.
rangeExactOtherCycle :: Term s (PInterval PPOSIXTime)
rangeExactOtherCycle = pinterval (pconstant 12_000) (pconstant 13_000)

-- TX range is inside interval in 6th cycle, which is off bounds
rangeInsideBadCycle :: Term s (PInterval PPOSIXTime)
rangeInsideBadCycle = pinterval (pconstant 27_000) (pconstant 28_000)

-- TX range starts too soon and ends inside interval
rangeStartsTooSoon :: Term s (PInterval PPOSIXTime)
rangeStartsTooSoon = pinterval (pconstant 1500) (pconstant 2500)

-- TX range starts inside interval but ends too late
rangeEndsTooLate :: Term s (PInterval PPOSIXTime)
rangeEndsTooLate = pinterval (pconstant 2500) (pconstant 3500)

-- TX range starts inside interval in cycle 3 and ends inside interval in
-- cycle 4. This range is too wide and should fail.
rangeTooWide :: Term s (PInterval PPOSIXTime)
rangeTooWide = pinterval (pconstant 2500) (pconstant 7500)