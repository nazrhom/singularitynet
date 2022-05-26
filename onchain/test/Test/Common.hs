module Test.Common (
  testStateCurrencySymbol,
  testListCurrencySymbol,
  testStatePolicy,
  testListPolicy,
  testStatePolicyInput,
  testAdminPkh,
) where

import ListNFT (plistNFTPolicy)
import StateNFT (pstateNFTPolicy)
import PTypes (PListAction)
import Plutarch.Api.V1 (PScriptContext, mintingPolicySymbol, mkMintingPolicy)
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  PubKeyHash (PubKeyHash),
  TokenName,
  TxId (TxId),
  TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
 )

-- The CurrencySymbol associated with the state NFT
testStateCurrencySymbol :: TokenName -> CurrencySymbol
testStateCurrencySymbol tn =
  mintingPolicySymbol $ mkMintingPolicy $ punsafeCoerce $ testStatePolicy tn

-- The CurrencySymbol associated with the association list NFT
testListCurrencySymbol :: TokenName -> CurrencySymbol
testListCurrencySymbol tn =
  mintingPolicySymbol $ mkMintingPolicy $ punsafeCoerce $ testListPolicy tn

-- | The pool's state minting policy
testStatePolicy ::
  forall (s :: S). TokenName -> Term s (PUnit :--> PScriptContext :--> PUnit)
testStatePolicy tn = pstateNFTPolicy tn # pconstant testStatePolicyInput

-- | The association list's minting policy
testListPolicy ::
  forall (s :: S).
  TokenName ->
  Term s (PListAction :--> PScriptContext :--> PUnit)
testListPolicy tn =
  plistNFTPolicy # pconstant (testStateCurrencySymbol tn)
    # pconstant tn

-- | The UTXO used to mint the pool's state NFT
testStatePolicyInput :: TxOutRef
testStatePolicyInput =
  TxOutRef
    { txOutRefId = TxId "ffffeeee"
    , txOutRefIdx = 0
    }

-- | The public key hash used by the pool administrator
testAdminPkh :: PubKeyHash
testAdminPkh = PubKeyHash "deadbeef"
