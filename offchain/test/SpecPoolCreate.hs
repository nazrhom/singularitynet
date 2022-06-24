{-# LANGUAGE RecordWildCards #-}

module SpecPoolCreate (
  specPoolCreate,
) where

import Data.Text (Text)

import Plutus.Contract (
  Contract,
  EmptySchema,
 )
import Plutus.V1.Ledger.Api (Value)
import Test.Plutip.Contract (assertExecution, withContract)
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (
  Predicate,
  shouldSucceed,
  yieldSatisfies,
 )
import Test.Tasty (TestTree)

import Ledger qualified as Ledger

import Plutus.V1.Ledger.Value (valueOf)
import SingularityNet.Settings (bondedStakingTokenName)
import SingularityNet.Types (BondedPoolParams (BondedPoolParams, nftCs))
import Test.Plutip.Contract.Values (valueAt)
import Types (
  BondedPoolScripts,
  BondedPoolTypedScripts (BondedPoolTypedScripts, validator),
  InitialBondedPoolParams,
 )
import Utils (
  createBondedPool,
  logInfo',
  testAdminWallet,
  testInitialBondedParams,
 )

specPoolCreate :: BondedPoolScripts -> TestTree
specPoolCreate scripts =
  Test.Plutip.LocalCluster.withCluster
    "Pool creation"
    [ assertExecution
        "should create the pool successfully and mint only once"
        testAdminWallet
        ( withContract . const $
            createPoolContract scripts testInitialBondedParams
        )
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
  (BondedPoolTypedScripts {..}, bpp, _) <-
    createBondedPool poolScripts ibpp
  -- Get pool value
  let poolAddr = Ledger.scriptAddress validator
  logInfo' "Getting validator UTxO..."
  val <- valueAt poolAddr
  pure (val, bpp)

shouldMintOnce :: Predicate w e (Value, BondedPoolParams)
shouldMintOnce = yieldSatisfies "mints once" $
  \(val, BondedPoolParams {..}) ->
    valueOf val nftCs bondedStakingTokenName == 5
