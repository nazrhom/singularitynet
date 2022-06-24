module SpecStateNFT (specStateNFT) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Void (Void)

import Plutus.V1.Ledger.Api (
  MintingPolicy (MintingPolicy),
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
  assertExecution,
  initAda,
  withContract,
 )
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)

import Ledger (ChainIndexTxOut, scriptCurrencySymbol)
import Ledger qualified as Ledger
import Ledger.Constraints (ScriptLookups)
import Ledger.Constraints.TxConstraints (TxConstraints)

import SingularityNet.Settings (bondedStakingTokenName)
import Utils (getOwnData, testAdminWallet)

specStateNFT :: Script -> TestTree
specStateNFT policyScript =
  Test.Plutip.LocalCluster.withCluster
    "State NFT Policy"
    [ assertExecution
        "should validate correct transaction"
        testAdminWallet
        (withContract . const $ nftMint 1)
        [shouldSucceed]
    , assertExecution
        "should not mint more than once"
        testAdminWallet
        (withContract . const $ nftMint 5)
        [shouldFail]
    , assertExecution
        "should not consume the wrong outRef"
        (initAda [5, 100])
        (withContract $ const wrongOutRefMint)
        [shouldFail]
    ]
  where
    nftMint :: Integer -> Contract String EmptySchema Text ()
    nftMint amt = do
      (_, utxos, outRef, policy) <- initContract
      let nftCs = scriptCurrencySymbol policy
          value = singleton nftCs bondedStakingTokenName amt

          lookups :: ScriptLookups Void
          lookups =
            Constraints.mintingPolicy policy
              <> Constraints.unspentOutputs utxos
          constraints :: TxConstraints Void Void
          constraints =
            Constraints.mustSpendPubKeyOutput outRef
              <> Constraints.mustMintValue value
      tx <- Contract.submitTxConstraintsWith lookups constraints
      Contract.awaitTxConfirmed . Tx.getCardanoTxId $ tx

    wrongOutRefMint :: Contract String EmptySchema Text ()
    wrongOutRefMint = do
      (_, utxos, outRef, policy) <- initContract
      Contract.logInfo $ "my utxos: " <> show utxos
      Contract.logInfo $ "state nft utxo: " <> show outRef
      -- There should only be two UTXOs in this wallet
      let wrongOutRef = Map.keys utxos !! 1
      Contract.logInfo $ "will spend this utxo: " <> show wrongOutRef

      let nftCs = scriptCurrencySymbol policy
          value = singleton nftCs bondedStakingTokenName 1

          lookups :: ScriptLookups Void
          lookups =
            Constraints.mintingPolicy policy
              <> Constraints.unspentOutputs utxos
          constraints :: TxConstraints Void Void
          constraints =
            Constraints.mustSpendPubKeyOutput wrongOutRef
              <> Constraints.mustMintValue value
      tx <- Contract.submitTxConstraintsWith lookups constraints
      Contract.awaitTxConfirmed . Tx.getCardanoTxId $ tx

    -- Common code used in all tests
    initContract ::
      Contract
        String
        EmptySchema
        Text
        ( PaymentPubKeyHash
        , Map TxOutRef ChainIndexTxOut
        , TxOutRef
        , MintingPolicy
        )
    initContract = do
      (pkh, _, utxos) <- getOwnData
      let outRef = head . Map.keys $ utxos
      pure (pkh, utxos, outRef, stateNFTPolicy outRef)

    -- Apply parameters of script
    stateNFTPolicy :: TxOutRef -> MintingPolicy
    stateNFTPolicy outRef =
      MintingPolicy $ applyArguments policyScript [PlutusTx.toData outRef]
