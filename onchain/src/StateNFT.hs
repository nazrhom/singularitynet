module StateNFT (
  pbondedStateNFTPolicy,
  pbondedStateNFTPolicyUntyped,
) where

{-
    This module implements a one-shot policy for minting NFTs as described in
    the spec for bonded pools.

    The policy is parametrized by a UTXO and a token name, the latter hardcoded
    in the `Settings` module.
-}

import Plutarch.Api.V1 (
  PScriptContext,
  PTxInInfo,
  PTxOutRef,
  PValue,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)

import Settings (bondedStakingTokenName)
import Utils (
  getCs,
  oneOf,
  ptryFromUndata,
 )

pbondedStateNFTPolicy ::
  forall (s :: S). Term s (PTxOutRef :--> PUnit :--> PScriptContext :--> PUnit)
pbondedStateNFTPolicy = plam $ \txOutRef _ ctx' -> P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  cs <- runTermCont $ getCs ctx.purpose
  txInfo <- pletFields @'["inputs", "mint", "id"] $ ctx.txInfo
  let mint :: Term s PValue
      mint = pfromData txInfo.mint
      inputs :: Term s (PBuiltinList (PAsData PTxInInfo))
      inputs = pfromData txInfo.inputs
  pif
    ( consumesRef # txOutRef # inputs
        #&& oneOf # cs # pconstant bondedStakingTokenName # mint
    )
    (pconstant ())
    perror

pbondedStateNFTPolicyUntyped ::
  forall (s :: S). Term s (PData :--> PData :--> PData :--> PUnit)
pbondedStateNFTPolicyUntyped = plam $ \utxo _ ctx' ->
  pbondedStateNFTPolicy # unTermCont (ptryFromUndata utxo)
    # pconstant ()
    # punsafeCoerce ctx'

consumesRef ::
  forall (s :: S).
  Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
consumesRef = phoistAcyclic $
  plam $ \txOutRef ->
    pany #$ plam $ \info -> pdata txOutRef #== pdata (getOutRef info)
  where
    getOutRef ::
      forall (s :: S).
      Term s (PAsData PTxInInfo) ->
      Term s PTxOutRef
    getOutRef info' = pfield @"outRef" # pfromData info'
