{-# LANGUAGE RecordWildCards #-}
module SpecUserStake(specUserStake) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)

import Plutus.V1.Ledger.Api (
  MintingPolicy (MintingPolicy),
  Script,
  TxOutRef,
  singleton, BuiltinByteString, PubKeyHash (PubKeyHash)
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
import Types (InitialBondedPoolParams, BondedPoolScripts, TBondedPool (TBondedPool))
import SingularityNet.Natural (Natural)

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
    (bpts, bpp, bondedPool) <- createBondedPool bps testInitialBondedParams 
    _ <- userHeadStake pkhs bpts bpp bondedPool stakeAmt
    pure ()
