module SpecUserStake(specUserStake) where

import Data.Text (Text, pack)

import Plutus.V1.Ledger.Api (
  POSIXTime,
 )

import Plutus.Contract (
  Contract,
  EmptySchema,
 )
import Plutus.Contract qualified as Contract
import Test.Plutip.Contract (
  assertExecution,
  initAda,
  withContract, TestWallet, TestWallets, withContractAs
 )
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)

import Ledger (ChainIndexTxOut, scriptCurrencySymbol, PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger qualified as Contract
import Ledger.Constraints (ScriptLookups)
import Ledger.Constraints.TxConstraints (TxConstraints)
import SingularityNet.Settings (bondedStakingTokenName)
import PlutusTx.Builtins (blake2b_256)

import Utils(testAdminWallet, testUserWallet, createBondedPool, testInitialBondedParams, userHeadStake, testUserStake, currentRoundedTime)
import SingularityNet.Types (BondedPoolParams)
import Types (InitialBondedPoolParams (initStart, initEnd, initIterations, initBondingLength, initUserLength), BondedPoolScripts, TBondedPool (TBondedPool))
import SingularityNet.Natural (Natural (Natural), toPOSIXTime)

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
userStakeContract bps stakeAmt pkhs  = do
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
        initialBondedParams = testInitialBondedParams {
            initStart = creationDelay,
            -- One extra userLength for the final withdrawing period
            initEnd = creationDelay + toPOSIXTime iterations * cycleLength + userLength,
            initUserLength = userLength,
            initBondingLength = bondingLength,
            initIterations = iterations
        }
    (bpts, bpp, bondedPool) <- createBondedPool bps initialBondedParams
    Contract.throwError $ pack $ "BondedPoolParams: " <> show bpp
    _ <- userHeadStake pkhs bpts bpp bondedPool stakeAmt
    pure ()

