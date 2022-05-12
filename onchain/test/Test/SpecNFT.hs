{-# LANGUAGE OverloadedStrings #-}

module Test.SpecNFT (nftTests) where

import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Api (
  Address (Address, addressCredential, addressStakingCredential),
  Credential (PubKeyCredential),
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
import Common.Settings (bondedStakingTokenName)
import Test.Common (
  testAdminPKH,
  testStateCurrencySymbol,
  testStatePolicy,
  testStatePolicyInput,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Utils (fails, succeeds)

nftTests :: TestTree
nftTests =
  testGroup
    "NFT tests"
    [ testCase "should validate correct transaction" $
        succeeds $ testStatePolicy # pconstant () # pconstant goodCtx1
    , testCase "should validate correct transaction with spurious tokens" $
        succeeds $ testStatePolicy # pconstant () # pconstant goodCtx2
    , testCase "should not mint more than once" $
        fails $ testStatePolicy # pconstant () # pconstant badCtx1
    , testCase "should not consume the wrong outRef" $
        fails $ testStatePolicy # pconstant () # pconstant badCtx2
    ]

-- Test data --

-- The address of all the the UTXOs involved (no validators are used)
testTxOutAddr :: Address
testTxOutAddr =
  Address
    { addressCredential = PubKeyCredential testAdminPKH
    , addressStakingCredential = Nothing
    }

-- The value contained by the previous UTXO
testInputTxOut :: TxOut
testInputTxOut =
  TxOut
    { txOutAddress = testTxOutAddr
    , txOutValue = lovelaceValueOf 50_000_000
    , txOutDatumHash = Nothing
    }

-- Contexts
goodCtx1 :: ScriptContext
goodCtx1 =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs =
              [ TxInInfo
                  { txInInfoOutRef = testStatePolicyInput
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
          , txInfoSignatories = [testAdminPKH]
          , txInfoData = []
          , txInfoId = TxId "abcdef12"
          }
    , scriptContextPurpose = Minting testStateCurrencySymbol
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
goodMint1 = singleton testStateCurrencySymbol bondedStakingTokenName 1

goodMint2 :: Value
goodMint2 =
  goodMint1
    <> singleton "ababab" "RandomTokenName1" 0
    <> singleton "efefef" "RandomTokenName2" 0

badMint1 :: Value
badMint1 = singleton testStateCurrencySymbol bondedStakingTokenName 10
