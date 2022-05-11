module Test.SpecBondedAdmin (
  bondedAdminTests,
) where

{-
   This modules tests administrator actions in the Bonded Pool. This includes
   timing errors.
-}

import BondedPool (pbondedPoolValidator, pbondedPoolValidatorUntyped)
import Data.Natural
import Data.Ratio
import Plutarch
import Plutarch.Api.V1 (
  PScriptContext,
  mintingPolicySymbol,
  mkMintingPolicy,
  validatorHash,
 )
import Plutarch.Builtin
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken, lovelaceValueOf)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Interval (interval)
import Settings (bondedStakingTokenName)
import StateNFT (pbondedStateNFTPolicy)
import Test.Common
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Utils (fails, succeeds)
import Types

bondedAdminTests :: TestTree
bondedAdminTests =
  testGroup
    "Admin Actions Tests"
    [ timingTests
    ]

timingTests :: TestTree
timingTests =
  testGroup
    "Timing Tests"
    [ testCase "should validate transaction with exact bonding timerange" $
        succeeds $ timingTestExact1
    ]

-- Common data --
redeemer :: BondedStakingAction
redeemer = AdminAct newSize

datum :: BondedStakingDatum
datum = StateDatum Nothing $ Natural 1_000_000_000

valHash :: ValidatorHash
valHash =
  validatorHash $
    Validator $
      compile $
        pbondedPoolValidatorUntyped # paramsData
  where
    paramsData :: forall (s :: S). Term s PData
    paramsData = pforgetData . pdata $ pconstant mkParameters

newSize :: Natural
newSize = Natural 800_000_000

newDatum :: BondedStakingDatum
newDatum = StateDatum Nothing newSize

-- The address of the pool
poolAddr :: Address
poolAddr =
  Address
    { addressCredential = ScriptCredential valHash
    , addressStakingCredential = Nothing
    }

-- Hashing the datum is not possible, so just a placeholder bytestring is used
poolStateDatumHash :: DatumHash
poolStateDatumHash = DatumHash $ "fafafafa"

poolStateNewDatumHash :: DatumHash
poolStateNewDatumHash = DatumHash $ "abababab"

poolStateTxOut :: TxOut
poolStateTxOut =
  TxOut
    { txOutAddress = poolAddr
    , txOutValue =
        lovelaceValueOf 50_000_000
          <> singleton testStateCurrencySymbol bondedStakingTokenName 1
    , txOutDatumHash = Just poolStateDatumHash
    }

poolStateTxOutRef :: TxOutRef
poolStateTxOutRef =
  TxOutRef
    { txOutRefId = TxId "abcd0001"
    , txOutRefIdx = 0
    }

-- An admin's UTXO with ADA
adminAddr :: Address
adminAddr =
  Address
    { addressCredential = PubKeyCredential testAdminPKH
    , addressStakingCredential = Nothing
    }

adminTxOut :: TxOut
adminTxOut =
  TxOut
    { txOutAddress = adminAddr
    , txOutValue = lovelaceValueOf 100_000_000
    , txOutDatumHash = Nothing
    }

adminTxOutRef :: TxOutRef
adminTxOutRef =
  TxOutRef
    { txOutRefId = TxId "ffff0000"
    , txOutRefIdx = 0
    }

-- The parameters of the pool. This pool has ADA as bonded asset.
mkParameters :: BondedPoolParams
mkParameters =
  BondedPoolParams
    { iterations = Natural 5
    , start = POSIXTime 1000
    , end = POSIXTime 17_000
    , userLength = POSIXTime 1000
    , bondingLength = POSIXTime 2000
    , interest = NatRatio $ 1 % 100
    , minStake = Natural 100
    , maxStake = Natural 500
    , admin = testAdminPKH
    , bondedAssetClass = AssetClass (adaSymbol, adaToken)
    , nftCs = testStateCurrencySymbol
    , assocListCs = testListCurrencySymbol
    }

mkContext :: POSIXTimeRange -> ScriptContext
mkContext range =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs =
              [ TxInInfo -- Pool's state UTXO
                  { txInInfoOutRef = poolStateTxOutRef
                  , txInInfoResolved = poolStateTxOut
                  }
              , TxInInfo -- Admin's UTXO with funds
                  { txInInfoOutRef = adminTxOutRef
                  , txInInfoResolved = adminTxOut
                  }
              ]
          , txInfoOutputs =
              [ TxOut -- Original value + Deposit + Updated Datum
                  { txOutAddress = poolAddr
                  , txOutValue =
                      txOutValue poolStateTxOut <> txOutValue adminTxOut
                  , txOutDatumHash = Just poolStateNewDatumHash
                  }
              ]
          , txInfoFee = lovelaceValueOf 1_000
          , txInfoMint = mempty
          , txInfoDCert = []
          , txInfoWdrl = []
          , txInfoValidRange = range
          , txInfoSignatories = [testAdminPKH]
          , txInfoData =
              [ (poolStateDatumHash, Datum $ toBuiltinData datum)
              , (poolStateNewDatumHash, Datum $ toBuiltinData newDatum)
              ]
          , txInfoId = TxId "abcdef12"
          }
    , scriptContextPurpose = Spending poolStateTxOutRef
    }

-- Timing Tests --
timingTestExact1 :: forall (s :: S). Term s PUnit
timingTestExact1 =
  pbondedPoolValidator
    # pconstant mkParameters
    # pconstant datum
    # pconstant redeemer
    # (pconstant . mkContext $ interval 1000 1999)
