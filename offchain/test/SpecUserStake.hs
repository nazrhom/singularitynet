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

import Utils(testAdminWallet, testUserWallet, createBondedPool, testInitialBondedParams, userHeadStake, testUserStake)
import SingularityNet.Types (BondedPoolParams)
import Types (InitialBondedPoolParams (initStart, initEnd, initIterations, initBondingLength, initUserLength), BondedPoolScripts, TBondedPool (TBondedPool))
import SingularityNet.Natural (Natural (Natural))

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
    currTime <- Contract.currentTime
    let userLength :: POSIXTime
        userLength = 180
        initialBondedParams :: InitialBondedPoolParams
        initialBondedParams = testInitialBondedParams {
            initStart = 80_000 + currTime,
            initEnd = 80_000 + currTime + 7 * userLength,
            initUserLength = userLength,
            initBondingLength = userLength,
            initIterations = Natural 1
        }
    (bpts, bpp, bondedPool) <- createBondedPool bps initialBondedParams
    _ <- userHeadStake pkhs bpts bpp bondedPool stakeAmt
    pure ()
