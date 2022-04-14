{-# LANGUAGE OverloadedStrings #-}
module SpecNFT where

import Utils
import Test.Tasty
import Test.Tasty.HUnit
import Settings(bondedStakingTokenName)
import NFT(pbondedStakingNFTPolicy)
import Plutarch.Api.V1
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Ada(lovelaceValueOf)
import Plutarch.Unsafe (punsafeCoerce)

nftTests :: TestTree
nftTests = testGroup
    "NFT tests"
    [ testCase "should mint once and consume the outRef" $
        succeeds $ testPolicy # pconstant () # pconstant goodCtx
    , testCase "should not mint more than once" $
        fails $ testPolicy # pconstant () # pconstant badCtx1]
        
-- Test data
testCurrencySymbol :: CurrencySymbol
testCurrencySymbol =
    mintingPolicySymbol $ mkMintingPolicy $ punsafeCoerce $ testPolicy
        
testPolicy :: forall (s :: S) . Term s (PUnit :--> PScriptContext :--> PUnit)
testPolicy = pbondedStakingNFTPolicy # pconstant testTxOutRef

testTxOutRef :: TxOutRef
testTxOutRef = TxOutRef {
    txOutRefId = TxId "ffffeeee",
    txOutRefIdx = 0
}

testTxOutAddr :: Address
testTxOutAddr = Address {
    addressCredential = PubKeyCredential testPubKeyHash,
    addressStakingCredential = Nothing
}

testTxInAddr :: Address
testTxInAddr = Address {
    addressCredential = PubKeyCredential testPubKeyHash,
    addressStakingCredential = Nothing
}

testPubKeyHash :: PubKeyHash
testPubKeyHash = PubKeyHash "deadbeef"

-- Contexts
goodCtx :: ScriptContext
goodCtx = ScriptContext {
    scriptContextTxInfo = TxInfo {
        txInfoInputs = [
            TxInInfo {
                txInInfoOutRef = testTxOutRef,
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
                txOutValue = singleton
                    testCurrencySymbol
                    bondedStakingTokenName
                    10,
                txOutDatumHash = Nothing
            }
        ]
    }
    , scriptContextPurpose = scriptContextPurpose goodCtx
}

goodMint :: Value
goodMint = singleton testCurrencySymbol bondedStakingTokenName 1
