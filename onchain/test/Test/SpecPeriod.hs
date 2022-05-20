module Test.SpecPeriod (
  specPeriodTests,
) where

{-
    module description here
-}

import SingularityNet.Natural (NatRatio (NatRatio), Natural (Natural))
import SingularityNet.Settings (bondedStakingTokenName)
import SingularityNet.Types (
  AssetClass (AssetClass),
  BondedPoolParams (
    BondedPoolParams,
    admin,
    assocListCs,
    bondedAssetClass,
    bondingLength,
    end,
    interest,
    iterations,
    maxStake,
    minStake,
    nftCs,
    start,
    userLength
  ),
 )
import PInterval (
  PPeriodicInterval (
    PPeriodicInterval,
    piBaseOffset,
    piEndOffset,
    piMaxCycles,
    piPeriod,
    piStartOffset
  ),
  getBondedPeriod,
  pinterval,
  pperiodicContains,
 )
import PTypes (
  PBondedPoolParams,
  PPeriod,
  bondingPeriod,
  closingPeriod,
  depositWithdrawPeriod,
  onlyWithdrawPeriod,
 )

import Plutarch.Api.V1 (PPOSIXTime)
import Plutarch.Api.V1.Interval (PInterval)

import Data.Ratio ((%))
import Plutus.V1.Ledger.Api (CurrencySymbol (CurrencySymbol))
import Test.Common (testAdminPkh)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Utils (fails, returnsFalse, returnsTrue, shouldBe)

specPeriodTests :: TestTree
specPeriodTests =
  testGroup
    "Period tests"
    [ pperiodTests
    , getBondedPeriodTests
    ]

pperiodTests :: TestTree
pperiodTests =
  testGroup
    "pperiodicContains tests"
    [ testCase "[2200; 2800) ∈ 1000 + [1000; 2000) % 5000" $
        returnsTrue $ testWithRange rangeInside
    , testCase "[2000; 3000) ∈ 1000 + [1000; 2000) % 5000" $
        returnsTrue $ testWithRange rangeExact
    , testCase "[12,000; 13,000) ∈ 1000 + 3 * [1000; 2000) % 5000" $
        returnsTrue $ testWithRange rangeExactOtherCycle
    , testCase "[27,000; 28,000) ∈ 1000 + [1000; 2000) % 5000" $
        returnsFalse $ testWithRange rangeInsideBadCycle
    , testCase "[1500; 2500) ∈ 1000 + [1000; 2000) % 5000" $
        returnsFalse $ testWithRange rangeStartsTooSoon
    , testCase "[2500; 3500) ∈ 1000 + [1000; 2000) % 5000" $
        returnsFalse $ testWithRange rangeEndsTooLate
    , testCase "[2500; 7500) ∈ 1000 + [1000; 2000) % 5000" $
        returnsFalse $ testWithRange rangeTooWide
    ]

getBondedPeriodTests :: TestTree
getBondedPeriodTests =
  testGroup
    "getBondedPeriod tests"
    [ testCase "valid deposit range" $
        getPeriod rangeDeposit `shouldBe` depositWithdrawPeriod
    , testCase "valid deposit range, exact" $
        getPeriod rangeDepositExact `shouldBe` depositWithdrawPeriod
    , testCase "invalid deposit, off by one" $
        fails $ getPeriod rangeDepositOffByOne
    , testCase "valid bonding range" $
        getPeriod rangeBonding `shouldBe` bondingPeriod
    , testCase "valid bonding range, exact" $
        getPeriod rangeBondingExact `shouldBe` bondingPeriod
    , testCase "invalid bonding range, off by one" $
        fails $ getPeriod rangeBondingOffByOne
    , testCase "last withdrawal range" $
        getPeriod rangeLastWithdrawal `shouldBe` onlyWithdrawPeriod
    , testCase "last withdrawal range, exact" $
        getPeriod rangeLastWithdrawalExact `shouldBe` onlyWithdrawPeriod
    , testCase "closing pool range" $
        getPeriod rangeClose `shouldBe` closingPeriod
    ]

---- Auxiliary functions ----
testWithRange ::
  forall (s :: S).
  Term s (PInterval PPOSIXTime) ->
  Term s PBool
testWithRange txRange = pperiodicContains # testPeriodicInterval # txRange

getPeriod ::
  forall (s :: S).
  Term s (PInterval PPOSIXTime) ->
  Term s PPeriod
getPeriod txRange = getBondedPeriod # txRange # testPoolParams

---- Test data ----
testPeriodicInterval :: forall (s :: S). Term s PPeriodicInterval
testPeriodicInterval =
  pcon $
    PPeriodicInterval
      { piBaseOffset = pconstant 1000
      , piPeriod = pconstant 5000
      , piStartOffset = pconstant 1000
      , piEndOffset = pconstant 2000
      , piMaxCycles = pconstant (Natural 5)
      }

testPoolParams :: forall (s :: S). Term s PBondedPoolParams
testPoolParams = pconstant params
  where
    params :: BondedPoolParams
    params =
      BondedPoolParams
        { iterations = Natural 3
        , start = 5000
        , end = 20_000
        , userLength = 3500
        , bondingLength = 1500
        , -- We are not testing any of the parameters below
          bondedAssetClass = AssetClass (nftCs params) bondedStakingTokenName
        , interest = NatRatio $ 1 % 100
        , minStake = Natural 50
        , maxStake = Natural 500
        , admin = testAdminPkh
        , nftCs = CurrencySymbol "deadbeef"
        , assocListCs = CurrencySymbol "abababab"
        }

-- Data for getBondedPeriod

-- TX range inside first deposit period
rangeDeposit :: Term s (PInterval PPOSIXTime)
rangeDeposit = pinterval (pconstant 5500) (pconstant 8000)

-- TX range exactly the same as first deposit period
rangeDepositExact :: Term s (PInterval PPOSIXTime)
rangeDepositExact = pinterval (pconstant 5000) (pconstant 8500)

-- TX range fails to be in first deposit period by 1 unit
rangeDepositOffByOne :: Term s (PInterval PPOSIXTime)
rangeDepositOffByOne = pinterval (pconstant 5001) (pconstant 8501)

-- TX range inside first bonding period
rangeBonding :: Term s (PInterval PPOSIXTime)
rangeBonding = pinterval (pconstant 9000) (pconstant 9800)

-- TX range exactly the same as first bonding period
rangeBondingExact :: Term s (PInterval PPOSIXTime)
rangeBondingExact = pinterval (pconstant 8500) (pconstant 10_000)

-- TX range fails to be in first deposit period by 1 unit
rangeBondingOffByOne :: Term s (PInterval PPOSIXTime)
rangeBondingOffByOne = pinterval (pconstant 8501) (pconstant 10_001)

-- TX range inside last withdrawal period
rangeLastWithdrawal :: Term s (PInterval PPOSIXTime)
rangeLastWithdrawal = pinterval (pconstant 21_000) (pconstant 22_500)

-- TX range is exactly last withdrawal period
rangeLastWithdrawalExact :: Term s (PInterval PPOSIXTime)
rangeLastWithdrawalExact = pinterval (pconstant 20_000) (pconstant 23_500)

-- TX range is in closing period
rangeClose :: Term s (PInterval PPOSIXTime)
rangeClose = pinterval (pconstant 30_000) (pconstant 50_000)

-- Data for pperiodTests

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
