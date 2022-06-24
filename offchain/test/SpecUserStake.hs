module SpecUserStake (specUserStake) where

import Data.Functor (void)
import Data.Text (Text, pack)

import Plutus.V1.Ledger.Api (
  POSIXTime,
 )

import Ledger (PaymentPubKeyHash)
import Plutus.Contract (
  Contract,
  EmptySchema,
 )
import Plutus.Contract qualified as Contract
import SingularityNet.Natural (Natural (Natural), toPOSIXTime)
import Test.Plutip.Contract (assertExecution, withContract)
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldSucceed)
import Test.Tasty (TestTree)
import Types (
  BondedPoolScripts,
  InitialBondedPoolParams (
    initBondingLength,
    initEnd,
    initIterations,
    initStart,
    initUserLength
  ),
 )
import Utils (
  createBondedPool,
  currentRoundedTime,
  testAdminWallet,
  testInitialBondedParams,
  testUserStake,
  testUserWallet,
  userHeadStake,
 )

specUserStake :: BondedPoolScripts -> TestTree
specUserStake scripts =
  Test.Plutip.LocalCluster.withCluster
    "Bonded Pool Validator - User Staking "
    [ assertExecution
        "should validate head stake (first of the pool)"
        (testAdminWallet <> testUserWallet)
        (withContract $ userStakeContract scripts testUserStake)
        [shouldSucceed]
    ]

userStakeContract ::
  BondedPoolScripts ->
  Natural ->
  [PaymentPubKeyHash] ->
  Contract String EmptySchema Text ()
userStakeContract bps stakeAmt pkhs = do
  -- Set up pool parameters
  currApproxTime <- currentRoundedTime
  let -- We give some time for the first transaction to run. `awaitTxConfirmed` waits for
      -- 8 blocks (if everything works correctly)
      creationDelay :: POSIXTime
      creationDelay = 80_000 + currApproxTime
      -- We let the users stake/withdraw for 20 seconds (unwise to make it smaller than this)
      userLength :: POSIXTime
      userLength = 20_000
      -- Same time for the operator to deposit
      bondingLength :: POSIXTime
      bondingLength = userLength
      cycleLength :: POSIXTime
      cycleLength = userLength + bondingLength
      iterations :: Natural
      iterations = Natural 1
      initialBondedParams :: InitialBondedPoolParams
      initialBondedParams =
        testInitialBondedParams
          { initStart = creationDelay
          , -- One extra userLength for the final withdrawing period
            initEnd =
              creationDelay + toPOSIXTime iterations * cycleLength + userLength
          , initUserLength = userLength
          , initBondingLength = bondingLength
          , initIterations = iterations
          }
  (bpts, bpp, bondedPool) <- createBondedPool bps initialBondedParams
  void $ Contract.throwError $ pack $ "BondedPoolParams: " <> show bpp
  void $ userHeadStake pkhs bpts bpp bondedPool stakeAmt
