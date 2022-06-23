{-# LANGUAGE RecordWildCards #-}

module SpecPoolCreate (
  specPoolCreate,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Void (Void)

import Plutus.V1.Ledger.Api (
  MintingPolicy (MintingPolicy),
  Script,
  TxOutRef,
  Value,
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
  assertExecution,
  initAda,
  withContract,
 )
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (Predicate, shouldFail, shouldSucceed, yieldSatisfies)
import Test.Tasty (TestTree)

import Ledger (ChainIndexTxOut, scriptCurrencySymbol)
import Ledger qualified as Ledger
import Ledger.Constraints (ScriptLookups)
import Ledger.Constraints.TxConstraints (TxConstraints)

import Data.Text qualified as T
import Plutus.V1.Ledger.Value (valueOf)
import SingularityNet.Settings (bondedStakingTokenName)
import SingularityNet.Types (BondedPoolParams (..), BondedStakingDatum)
import Test.Plutip.Contract.Values (valueAt)
import Types (BondedPoolScripts, BondedPoolTypedScripts (..), InitialBondedPoolParams)
import Utils (createBondedPool, getFirstUtxo, getOwnData, logInfo, logInfo', testAdminWallet, testInitialBondedParams)

specPoolCreate :: BondedPoolScripts -> TestTree
specPoolCreate scripts =
  Test.Plutip.LocalCluster.withCluster
    "Pool creation"
    [ assertExecution
        "should create the pool successfully and mint only once"
        testAdminWallet
        (withContract . const $ createPoolContract scripts testInitialBondedParams)
        [ shouldSucceed
        , shouldMintOnce
        ]
    ]

createPoolContract ::
  BondedPoolScripts ->
  InitialBondedPoolParams ->
  Contract String EmptySchema Text (Value, BondedPoolParams)
createPoolContract poolScripts ibpp = do
  -- Create value
  (BondedPoolTypedScripts {..}, bpp, _) <- createBondedPool poolScripts ibpp
  -- Get pool value
  let poolAddr = Ledger.scriptAddress validator
  logInfo' "Getting validator UTxO..."
  val <- valueAt poolAddr
  pure (val, bpp)

shouldMintOnce :: Predicate w e (Value, BondedPoolParams)
shouldMintOnce = yieldSatisfies "mints once" $ \(val, BondedPoolParams {..}) ->
  valueOf val nftCs bondedStakingTokenName == 5
