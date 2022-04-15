{-# LANGUAGE OverloadedStrings #-}
module SpecNFT(nftTests) where

import Utils ( succeeds, fails )
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase )
import Settings(bondedStakingTokenName)
import NFT(pbondedStakingNFTPolicy)
import Plutarch.Api.V1
    ( PScriptContext, mintingPolicySymbol, mkMintingPolicy )
import Plutus.V1.Ledger.Api
    ( singleton,
      CurrencySymbol,
      Value,
      Address(addressCredential, addressStakingCredential, Address),
      Credential(PubKeyCredential),
      PubKeyHash(PubKeyHash),
      ScriptContext (ScriptContext, scriptContextTxInfo, scriptContextPurpose),
      ScriptPurpose(Minting),
      TxInInfo(TxInInfo, txInInfoResolved, txInInfoOutRef),
      TxInfo(TxInfo, txInfoInputs, txInfoFee, txInfoDCert, txInfoWdrl,
             txInfoValidRange, txInfoSignatories, txInfoData, txInfoId,
             txInfoOutputs, txInfoMint),
      TxOut(TxOut, txOutAddress, txOutValue, txOutDatumHash),
      TxOutRef (txOutRefId, txOutRefIdx, TxOutRef),
      TxId(TxId),
      always )
import Plutus.V1.Ledger.Ada(lovelaceValueOf)
import Plutarch.Unsafe (punsafeCoerce)

nftTests :: TestTree
nftTests = testGroup
    "NFT tests"
    [ testCase "should validate correct transaction" $
        succeeds $ testPolicy # pconstant () # pconstant goodCtx
    , testCase "should not mint more than once" $
        fails $ testPolicy # pconstant () # pconstant badCtx1
    , testCase "should not consume the wrong outRef" $
        fails $ testPolicy # pconstant () # pconstant badCtx2]

-- Test data --

-- The CurrencySymbol associated with the policy
testCurrencySymbol :: CurrencySymbol
testCurrencySymbol =
    mintingPolicySymbol $ mkMintingPolicy $ punsafeCoerce $ testPolicy
        
-- The policy
testPolicy :: forall (s :: S) . Term s (PUnit :--> PScriptContext :--> PUnit)
testPolicy = pbondedStakingNFTPolicy # pconstant testInputTxOutRef

-- The UTXO used to mint the NFT
testInputTxOutRef :: TxOutRef
testInputTxOutRef = TxOutRef {
    txOutRefId = TxId "ffffeeee",
    txOutRefIdx = 0
}

-- The value contained by the previous UTXO
testInputTxOut :: TxOut
testInputTxOut = TxOut {
    txOutAddress = testTxOutAddr,
    txOutValue = lovelaceValueOf 50_000_000,
    txOutDatumHash = Nothing
}

-- The address of all the the UTXOs involved (no validators are used)
testTxOutAddr :: Address
testTxOutAddr = Address {
    addressCredential = PubKeyCredential testPubKeyHash,
    addressStakingCredential = Nothing
}

-- The public key hash used for signing
testPubKeyHash :: PubKeyHash
testPubKeyHash = PubKeyHash "deadbeef"

-- Contexts
goodCtx :: ScriptContext
goodCtx = ScriptContext {
    scriptContextTxInfo = TxInfo {
        txInfoInputs = [
            TxInInfo {
                txInInfoOutRef = testInputTxOutRef,
                txInInfoResolved = TxOut {
                    txOutAddress = testTxOutAddr,
                    txOutValue = lovelaceValueOf 50_000_000,
                    txOutDatumHash = Nothing
                }
            }
        ]
        , txInfoOutputs = [
            TxOut {
                txOutAddress = testTxOutAddr,
                txOutValue = goodMint,
                txOutDatumHash = Nothing
            }
        ]
        , txInfoFee = lovelaceValueOf 1_000
        , txInfoMint = goodMint
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = always
        , txInfoSignatories = [testPubKeyHash]
        , txInfoData = []
        , txInfoId = TxId "abcdef12"
    },
    scriptContextPurpose = Minting testCurrencySymbol
}

-- More than one token minted
badCtx1 :: ScriptContext
badCtx1 = goodCtx {
    scriptContextTxInfo = (scriptContextTxInfo goodCtx) {
        txInfoOutputs = [
            TxOut {
                txOutAddress = testTxOutAddr,
                txOutValue = badMint1,
                txOutDatumHash = Nothing
            }
        ]
        , txInfoMint = badMint1
    }
    , scriptContextPurpose = scriptContextPurpose goodCtx
}

-- The transaction does not consume the NFT's matching UTXO
badCtx2 :: ScriptContext
badCtx2 = goodCtx {
    scriptContextTxInfo = (scriptContextTxInfo goodCtx) {
        txInfoInputs = [
            TxInInfo {
                -- Wrong OutRef
                txInInfoOutRef = TxOutRef {
                    txOutRefId = TxId "dddddddd",
                    txOutRefIdx = 0
                },
                txInInfoResolved = testInputTxOut
            }
        ]
    }
    , scriptContextPurpose = scriptContextPurpose goodCtx
}

goodMint :: Value
goodMint = singleton testCurrencySymbol bondedStakingTokenName 1

badMint1 :: Value
badMint1 = singleton testCurrencySymbol bondedStakingTokenName 10