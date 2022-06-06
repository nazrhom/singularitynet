module Test.SpecPeriod (
  specPeriodTests,
) where

{-
    module description here
-}

import BondedStaking.PTypes (PBondedPoolParams)

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
  PPeriod,
  bondingPeriod,
  closingPeriod,
  depositWithdrawPeriod,
  onlyWithdrawPeriod,
 )
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

import Plutarch.Api.V1 (PPOSIXTime)
import Plutarch.Api.V1.Interval (PInterval)

import Data.Ratio ((%))
import Plutus.V1.Ledger.Api (CurrencySymbol (CurrencySymbol), POSIXTime)
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
    [ testCase "range inside" $
        returnsTrue $ testWithRange rangeInside
    , testCase "range inside and exactly the same" $
        returnsTrue $ testWithRange rangeExact
    , testCase "range is exactly the same, third cycle" $
        returnsTrue $ testWithRange rangeExactOtherCycle
    , testCase "range is inside, but the cycle exceeds the maximum" $
        returnsFalse $ testWithRange rangeInsideBadCycle
    , testCase "range starts too soon" $
        returnsFalse $ testWithRange rangeStartsTooSoon
    , testCase "range ends too late" $
        returnsFalse $ testWithRange rangeEndsTooLate
    , testCase "range starts in cycle 3 and ends in cycle 4, too wide" $
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

mkInterval ::
  forall (s :: S). POSIXTime -> POSIXTime -> Term s (PInterval PPOSIXTime)
mkInterval start end = pinterval (pconstant start) (pconstant end)

---- Test data ----
testPeriodicInterval :: forall (s :: S). Term s PPeriodicInterval
testPeriodicInterval =
  pcon $
    PPeriodicInterval
      { piBaseOffset = pconstant 1000
      , piPeriod = pconstant 5000
      , piStartOffset = pconstant 1000
      , piEndOffset = pconstant 2000
      , piMaxCycles = pcon $ PJust $ pconstant (Natural 5)
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
rangeDeposit :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeDeposit = mkInterval 5500 8000

-- TX range exactly the same as first deposit period
rangeDepositExact :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeDepositExact = mkInterval 5000 8500

-- TX range fails to be in first deposit period by 1 unit
rangeDepositOffByOne :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeDepositOffByOne = mkInterval 5001 8501

-- TX range inside first bonding period
rangeBonding :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeBonding = mkInterval 9000 9800

-- TX range exactly the same as first bonding period
rangeBondingExact :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeBondingExact = mkInterval 8500 10_000

-- TX range fails to be in first deposit period by 1 unit
rangeBondingOffByOne :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeBondingOffByOne = mkInterval 8501 10_001

-- TX range inside last withdrawal period
rangeLastWithdrawal :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeLastWithdrawal = mkInterval 21_000 22_500

-- TX range is exactly last withdrawal period
rangeLastWithdrawalExact :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeLastWithdrawalExact = mkInterval 20_000 23_500

-- TX range is in closing period
rangeClose :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeClose = mkInterval 30_000 50_000

-- Data for pperiodTests

-- TX range is inside interval in first cycle
rangeInside :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeInside = mkInterval 2200 2800

-- TX range is *exactly* the interval in first cycle
rangeExact :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeExact = mkInterval 2000 3000

-- TX range is inside interval in 3rd cycle. It is also exact.
rangeExactOtherCycle :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeExactOtherCycle = mkInterval 12_000 13_000

-- TX range is inside interval in 6th cycle, which is off bounds
rangeInsideBadCycle :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeInsideBadCycle = mkInterval 27_000 28_000

-- TX range starts too soon and ends inside interval
rangeStartsTooSoon :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeStartsTooSoon = mkInterval 1500 2500

-- TX range starts inside interval but ends too late
rangeEndsTooLate :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeEndsTooLate = mkInterval 2500 3500

-- TX range starts inside interval in cycle 3 and ends inside interval in
-- cycle 4. This range is too wide and should fail.
rangeTooWide :: forall (s :: S). Term s (PInterval PPOSIXTime)
rangeTooWide = mkInterval 2500 7500
