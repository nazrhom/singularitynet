{-# LANGUAGE OverloadedStrings #-}

module Test.SpecNFT (nftTests) where

import NFT (pbondedStakingNFTPolicy)
import Plutarch.Api.V1 (
  PScriptContext,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Api (
  Address (Address, addressCredential, addressStakingCredential),
  Credential (PubKeyCredential),
  CurrencySymbol,
  PubKeyHash (PubKeyHash),
  ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TxId (TxId),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (
    TxInfo,
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
  Value,
  always,
  singleton,
 )
import Settings (bondedStakingTokenName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Utils (fails, succeeds)

nftTests :: TestTree
nftTests =
  testGroup
    "NFT tests"
    [ testCase "should validate correct transaction" $
        succeeds $ testPolicy # pconstant () # pconstant goodCtx1
    , testCase "should validate correct transaction with spurious tokens" $
        succeeds $ testPolicy # pconstant () # pconstant goodCtx2
    , testCase "should not mint more than once" $
        fails $ testPolicy # pconstant () # pconstant badCtx1
    , testCase "should not consume the wrong outRef" $
        fails $ testPolicy # pconstant () # pconstant badCtx2
    ]

-- Test data --

-- The CurrencySymbol associated with the policy
testCurrencySymbol :: CurrencySymbol
testCurrencySymbol =
  mintingPolicySymbol $ mkMintingPolicy $ punsafeCoerce $ testPolicy

-- The policy
testPolicy :: forall (s :: S). Term s (PUnit :--> PScriptContext :--> PUnit)
testPolicy = pbondedStakingNFTPolicy # pconstant testInputTxOutRef

-- The UTXO used to mint the NFT
testInputTxOutRef :: TxOutRef
testInputTxOutRef =
  TxOutRef
    { txOutRefId = TxId "ffffeeee"
    , txOutRefIdx = 0
    }

-- The value contained by the previous UTXO
testInputTxOut :: TxOut
testInputTxOut =
  TxOut
    { txOutAddress = testTxOutAddr
    , txOutValue = lovelaceValueOf 50_000_000
    , txOutDatumHash = Nothing
    }

-- The address of all the the UTXOs involved (no validators are used)
testTxOutAddr :: Address
testTxOutAddr =
  Address
    { addressCredential = PubKeyCredential testPubKeyHash
    , addressStakingCredential = Nothing
    }

-- The public key hash used for signing
testPubKeyHash :: PubKeyHash
testPubKeyHash = PubKeyHash "deadbeef"

-- Contexts
goodCtx1 :: ScriptContext
goodCtx1 =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs =
              [ TxInInfo
                  { txInInfoOutRef = testInputTxOutRef
                  , txInInfoResolved = testInputTxOut
                  }
              ]
          , txInfoOutputs =
              [ TxOut
                  { txOutAddress = testTxOutAddr
                  , txOutValue = goodMint1
                  , txOutDatumHash = Nothing
                  }
              ]
          , txInfoFee = lovelaceValueOf 1_000
          , txInfoMint = goodMint1
          , txInfoDCert = []
          , txInfoWdrl = []
          , txInfoValidRange = always
          , txInfoSignatories = [testPubKeyHash]
          , txInfoData = []
          , txInfoId = TxId "abcdef12"
          }
    , scriptContextPurpose = Minting testCurrencySymbol
    }

-- It's good, but contains spurious tokens with zero quantity in its mint field
goodCtx2 :: ScriptContext
goodCtx2 =
  goodCtx1
    { scriptContextTxInfo =
        (scriptContextTxInfo goodCtx1)
          { txInfoMint = goodMint2
          }
    }

-- More than one token minted
badCtx1 :: ScriptContext
badCtx1 =
  goodCtx1
    { scriptContextTxInfo =
        (scriptContextTxInfo goodCtx1)
          { txInfoOutputs =
              [ TxOut
                  { txOutAddress = testTxOutAddr
                  , txOutValue = badMint1
                  , txOutDatumHash = Nothing
                  }
              ]
          , txInfoMint = badMint1
          }
    }

-- The transaction does not consume the NFT's matching UTXO
badCtx2 :: ScriptContext
badCtx2 =
  goodCtx1
    { scriptContextTxInfo =
        (scriptContextTxInfo goodCtx1)
          { txInfoInputs =
              [ TxInInfo
                  { -- Wrong OutRef
                    txInInfoOutRef =
                      TxOutRef
                        { txOutRefId = TxId "dddddddd"
                        , txOutRefIdx = 0
                        }
                  , txInInfoResolved = testInputTxOut
                  }
              ]
          }
    }

goodMint1 :: Value
goodMint1 = singleton testCurrencySymbol bondedStakingTokenName 1

goodMint2 :: Value
goodMint2 =
  goodMint1
    <> singleton "ababab" "RandomTokenName1" 0
    <> singleton "efefef" "RandomTokenName2" 0

badMint1 :: Value
badMint1 = singleton testCurrencySymbol bondedStakingTokenName 10
