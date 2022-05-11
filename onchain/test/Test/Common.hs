module Test.Common (
  testStateCurrencySymbol,
  testListCurrencySymbol,
  testStatePolicy,
  testListPolicy,
  testStatePolicyInput,
  testAdminPKH,
) where

import ListNFT (pbondedListNFTPolicy)
import Plutarch.Api.V1
import Plutarch.Unsafe
import Plutus.V1.Ledger.Ada
import Plutus.V1.Ledger.Api
import StateNFT (pbondedStateNFTPolicy)
import Types (PMintingAction)

-- The CurrencySymbol associated with the state NFT
testStateCurrencySymbol :: CurrencySymbol
testStateCurrencySymbol =
  mintingPolicySymbol $ mkMintingPolicy $ punsafeCoerce $ testStatePolicy

-- The CurrencySymbol associated with the associacion list NFT
testListCurrencySymbol :: CurrencySymbol
testListCurrencySymbol =
  mintingPolicySymbol $ mkMintingPolicy $ punsafeCoerce $ testListPolicy

-- | The pool's state minting policy
testStatePolicy :: forall (s :: S). Term s (PUnit :--> PScriptContext :--> PUnit)
testStatePolicy = pbondedStateNFTPolicy # pconstant testStatePolicyInput

-- | The association list's minting policy
testListPolicy :: forall (s :: S). Term s (PMintingAction :--> PScriptContext :--> PUnit)
testListPolicy = pbondedListNFTPolicy # pconstant testStateCurrencySymbol

-- | The UTXO used to mint the pool's state NFT
testStatePolicyInput :: TxOutRef
testStatePolicyInput =
  TxOutRef
    { txOutRefId = TxId "ffffeeee"
    , txOutRefIdx = 0
    }

-- | The public key hash used by the pool administrator
testAdminPKH :: PubKeyHash
testAdminPKH = PubKeyHash "deadbeef"
