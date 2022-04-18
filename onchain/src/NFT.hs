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
import Plutarch.Api.V1.Value (PTokenName)
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Api (MintingPolicy)
import Plutus.V1.Ledger.Tx (TxOutRef)
import Settings (bondedStakingTokenName)

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
        #&& mintsOneToken # cs # mint
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

mintsOneToken ::
  forall (s :: S).
  Term s (PCurrencySymbol :--> PValue :--> PBool)
mintsOneToken = phoistAcyclic $
  plam $ \cs val -> P.do
    -- The map of CurrencySymbols
    let csMap = pto $ pto val
    -- The value must contain only one currency symbol
    csPair <- oneKey csMap
    -- The currency symbol must match
    tnMap <- csMatch cs csPair
    -- The token name must be just one
    tnPair <- oneKey (pto tnMap)
    -- The token name must match and the amount must be 1
    tnMatchOne tnPair
  where
    oneKey ::
      forall (s :: S) (a :: PType).
      PLift a =>
      Term s (PBuiltinList a) ->
      (Term s a -> Term s PBool) ->
      Term s PBool
    oneKey val cont = pmatch val $ \case
      PCons p ps ->
        pif
          (pnull # ps)
          (cont p)
          (ptrace "too many CurrencySymbols/TokenNames" $ pconstant False)
      PNil -> ptrace "empty map inside Value" $ pconstant False
    csMatch ::
      forall (s :: S) (a :: PType).
      PIsData a =>
      Term s PCurrencySymbol ->
      Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData a)) ->
      (Term s a -> Term s PBool) ->
      Term s PBool
    csMatch cs pair cont =
      pif
        (pfst pair #== cs)
        (cont $ psnd pair)
        (ptrace "currency symbol does not match" $ pconstant False)
    tnMatchOne ::
      forall (s :: S).
      Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) ->
      Term s PBool
    tnMatchOne pair =
      pif
        (pfst pair #== pconstant bondedStakingTokenName #&& psnd pair #== 1)
        (pconstant True)
        (ptrace "token name does not match / too many tokens" $ pconstant False)
    pfst p = pfromData $ pfstBuiltin # p
    psnd p = pfromData $ psndBuiltin # p
