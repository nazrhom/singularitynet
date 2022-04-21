module NFT (
  pbondedStakingNFTPolicy,
  hbondedStakingNFTPolicy,
) where

{-
    This module implements a one-shot policy for minting NFTs as described in
    the spec for bonded pools.

    The policy is parametrized by a UTXO and a token name, the latter hardcoded
    in the `Settings` module.
-}

import Plutarch.Api.V1 (
  PCurrencySymbol,
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxInInfo,
  PTxOutRef,
  PValue,
  mkMintingPolicy,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Api (MintingPolicy)
import Plutus.V1.Ledger.Tx (TxOutRef)

import Settings (bondedStakingTokenName)
import Utils (oneOf)

pbondedStakingNFTPolicy ::
  forall (s :: S). Term s (PTxOutRef :--> PUnit :--> PScriptContext :--> PUnit)
pbondedStakingNFTPolicy = plam $ \txOutRef _ ctx' -> P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  cs <- getCs ctx.purpose
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

hbondedStakingNFTPolicy :: TxOutRef -> MintingPolicy
hbondedStakingNFTPolicy utxo =
  mkMintingPolicy $ punsafeCoerce $ pbondedStakingNFTPolicy # pconstant utxo

-- Gets the currency symbol of the script (equivalent to ownCurrencySymbol)
getCs ::
  forall (s :: S).
  Term s PScriptPurpose ->
  (Term s PCurrencySymbol -> Term s PUnit) ->
  Term s PUnit
getCs purpose cont = P.do
  pmatch purpose $ \case
    PMinting cs' -> cont $ pfield @"_0" # cs'
    _ -> ptrace "not a minting transaction" perror

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
