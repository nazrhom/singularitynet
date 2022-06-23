module SpecPoolDeposit (
  specPoolDeposit,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)

import Plutus.V1.Ledger.Api (
  BuiltinByteString,
  MintingPolicy (MintingPolicy),
  PubKeyHash (PubKeyHash),
  Script,
  TxOutRef,
  singleton,
 )
import Plutus.V1.Ledger.Scripts (
  applyArguments,
 )
import PlutusTx (toData)

import Ledger.Address (Address, PaymentPubKeyHash)
import Ledger.Constraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Plutus.Contract (
  Contract,
  EmptySchema,
 )
import Plutus.Contract qualified as Contract
import Test.Plutip.Contract (
  TestWallet,
  TestWallets,
  assertExecution,
  initAda,
  withContract,
  withContractAs,
 )
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)

import Ledger (ChainIndexTxOut, PaymentPubKeyHash (PaymentPubKeyHash), scriptCurrencySymbol)
import Ledger qualified as Contract
import Ledger.Constraints (ScriptLookups)
import Ledger.Constraints.TxConstraints (TxConstraints)
import PlutusTx.Builtins (blake2b_256)
import SingularityNet.Settings (bondedStakingTokenName)

import SingularityNet.Natural (Natural (Natural))
import SingularityNet.Types (BondedPoolParams)
import Types (BondedPoolScripts, InitialBondedPoolParams, TBondedPool (TBondedPool))
import Utils (createBondedPool, poolDeposit, testAdminDeposit, testAdminWallet, testInitialBondedParams, testUserStake, testUserWallet, userHeadStake)

specPoolDeposit :: BondedPoolScripts -> TestTree
specPoolDeposit scripts =
  Test.Plutip.LocalCluster.withCluster
    "Bonded Pool Validator - Pool depositing"
    [ assertExecution
        "should validate deposit"
        (testAdminWallet <> testUserWallet)
        (withContract $ poolDepositContract scripts testUserStake testAdminDeposit)
        [shouldSucceed]
    ]

poolDepositContract ::
  BondedPoolScripts ->
  Natural ->
  Natural ->
  [PaymentPubKeyHash] ->
  Contract String EmptySchema Text ()
poolDepositContract bps stakeAmt depositAmt pkhs = do
  (bpts, bpp, bondedPool) <- createBondedPool bps testInitialBondedParams
  (bondedPool', entry) <- userHeadStake pkhs bpts bpp bondedPool stakeAmt
  _ <- poolDeposit bpts bpp bondedPool' [entry] depositAmt
  pure ()
