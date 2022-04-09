module NFT
  ( pbondedStakingNFTPolicy,
    hbondedStakingNFTPolicy,
    bondedStakingNFTPolicyHash,
  )
where

{-
    This module implements a one-shot policy for minting NFTs as described in the
    spec for bonded pools.

    The policy is parametrized by a UTXO and a token name, the latter hardcoded in
    the `Settings` module.
-}

import Plutarch (compile)
import Plutarch.Api.V1
  ( PCurrencySymbol (PCurrencySymbol),
    PScriptContext,
    PScriptPurpose (PMinting),
    PTxInInfo,
    PTxOutRef,
    PValue,
    scriptHash,
  )
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Api (Script)
import Plutus.V1.Ledger.Scripts (ScriptHash)
import Plutus.V1.Ledger.Tx (TxOutRef)
import Settings (bondedStakingTokenName)

pbondedStakingNFTPolicy :: forall (s :: S). Term s (PTxOutRef :--> PUnit :--> PScriptContext :--> PUnit)
pbondedStakingNFTPolicy = plam $ \txOutRef _ ctx' -> P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  txInfo <- pletFields @'["inputs", "mint", "id"] $ ctx.txInfo
  let cs :: Term s PCurrencySymbol
      cs = pcon $ PCurrencySymbol $ pfield @"_0" #$ txInfo.id
      mint :: Term s PValue
      mint = pfromData txInfo.mint
      inputs :: Term s (PBuiltinList (PAsData PTxInInfo))
      inputs = pfromData txInfo.inputs
  pif
    ( isMinting # ctx.purpose
        #&& consumesRef # txOutRef # inputs
        #&& mintsOneToken # cs # mint
    )
    (pconstant ())
    perror

hbondedStakingNFTPolicy :: TxOutRef -> Script
hbondedStakingNFTPolicy utxo = compile $ pbondedStakingNFTPolicy # pconstant utxo

bondedStakingNFTPolicyHash :: TxOutRef -> ScriptHash
bondedStakingNFTPolicyHash utxo = scriptHash $ hbondedStakingNFTPolicy utxo

isMinting :: Term s (PScriptPurpose :--> PBool)
isMinting = plam $
  flip pmatch $ \case
    (PMinting _) -> pconstant True
    _ -> ptrace "can only mint in minting transaction" $ pconstant False

consumesRef :: Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PBool)
consumesRef = plam $ \txOutRef ->
  pany #$ plam $ \info -> pdata txOutRef #== pdata (getOutRef info)
  where
    getOutRef :: Term s (PAsData PTxInInfo) -> Term s PTxOutRef
    getOutRef info' = pfield @"outRef" # pfromData info'

mintsOneToken :: forall (s :: S). Term s (PCurrencySymbol :--> PValue :--> PBool)
mintsOneToken = plam $ \cs val -> P.do
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
    oneKey val cont = pmatch val $ \case
      (PCons p ps) ->
        pif
          (pnull # ps)
          (cont p)
          (ptrace "too many CurrencySymbols/TokenNames" $ pconstant False)
      PNil -> ptrace "empty map inside Value" $ pconstant False
    csMatch cs pair cont =
      pif
        (pfst pair #== cs)
        (cont $ psnd pair)
        (ptrace "currency symbol does not match" $ pconstant False)
    tnMatchOne pair =
      pif
        (pfst pair #== pconstant bondedStakingTokenName #&& psnd pair #== 1)
        (pconstant True)
        (ptrace "token name does not match" $ pconstant False)
    pfst p = pfromData $ pfstBuiltin # p
    psnd p = pfromData $ psndBuiltin # p