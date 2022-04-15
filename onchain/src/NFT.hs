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
  PCurrencySymbol
  , PScriptContext
  , PScriptPurpose (PMinting)
  , PTxInInfo
  , PTxOutRef
  , PValue
  , mkMintingPolicy
 )
import Plutarch.Api.V1.Value (PTokenName)
import Plutarch.Monadic qualified as P
import Plutus.V1.Ledger.Api (MintingPolicy)
import Plutus.V1.Ledger.Tx (TxOutRef)
import Settings (bondedStakingTokenName)
import Plutarch.Unsafe (punsafeCoerce)

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

oneOf :: forall (s :: S). Term s (
  PCurrencySymbol :--> PTokenName :--> PValue :--> PBool)
oneOf = phoistAcyclic $ plam $ \cs tn val ->
  oneOfWith
    # (peq # cs)
    # (peq # tn)
    # (ple # 1)
    #$ val 
  where ple :: forall (s :: S) . Term s (PInteger :--> PInteger :--> PBool)
        ple = plam $ \lim x -> x #<= lim
        
-- Returns True if only *one* token satisfies the three predicates given
oneOfWith :: forall (s :: S) . Term s (
  (PCurrencySymbol :--> PBool) :-->
  (PTokenName :--> PBool) :-->
  (PInteger :--> PBool) :-->
  PValue :-->
  PBool)
oneOfWith = phoistAcyclic $ plam $ \csPred tnPred nPred ->
  tokenPredicate pxor csPred tnPred nPred
  where pxor :: forall (s :: S) . Term s (PBool :--> PBool :--> PBool)
        pxor = plam $ \x y -> pnot #$ peq # pdata x # pdata y

peq :: forall (s :: S) (a :: PType) . PEq a => Term s (a :--> a :--> PBool)
peq = plam $ \x y -> x #== y


-- Assigns a boolean to each token in the value based on the the result of:
--
-- > csPred cs `pand` tnPred tn `pand` amountPred n
--
-- where each token is a tuple `(cs, tn, n)`.
-- 
-- Then, all the booleans are combined according to the boolean operator `op`.
--
-- This allows short-circuiting evaluation (e.g: a failure in `csPred`
-- avoids evaluating the remaining predicates). `op` can be any binary boolean
-- operator, like `pxor` (if only one token needs to satisfy all predicates) or
-- `pand` (if all tokens must satisfy the predicates).
-- 
-- However, this generic function does not allow short-circuiting row-wise,
-- meaning the `op` is strict in both arguments.
tokenPredicate :: forall s .
    Term s (PBool :--> PBool :--> PBool) ->
    Term s (PCurrencySymbol :--> PBool) ->
    Term s (PTokenName :--> PBool) ->
    Term s (PInteger :--> PBool) ->
    Term s (PValue :--> PBool)
tokenPredicate boolOp csPred tnPred nPred = plam $ \val -> P.do
    -- Map of CurrencySymbols
    let csMap = pto $ pto val
    csTnPair <- matchPair csMap
    tnMap <- evalCs csTnPair
    tnAmountPair <- matchPair $ pto tnMap
    evalTnAndAmount tnAmountPair
    where 
          matchPair :: forall (a :: PType) . PLift a =>
            Term s (PBuiltinList a) ->
            (Term s a -> Term s PBool) ->
            Term s PBool
          matchPair ls cont = (pfix # plam go) # ls # plam cont
            where -- Recurring function for the Y combinator
              go :: forall (a :: PType) . PLift a =>
                Term s (PBuiltinList a :--> (a :--> PBool) :--> PBool) ->
                Term s (PBuiltinList a) ->
                Term s (a :--> PBool) ->
                Term s PBool
              go self ls cont = pmatch ls $ \case
                PNil -> pconstant False
                PCons p ps -> boolOp # (cont # p) #$ self # ps # cont
          evalCs :: forall (a :: PType) .
            PIsData a =>
            Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData a)) ->
            (Term s a -> Term s PBool) ->
            (Term s PBool)
          evalCs pair cont =
            pif
              (csPred # pfst pair)
              (ptrace "evalCs OK" $ cont $ psnd pair)
              (ptrace "predicate on CurrencySymbol not satisfied" $
                pconstant False)
          evalTnAndAmount ::
            Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) ->
            Term s PBool
          evalTnAndAmount pair =
            pif
              (tnPred # pfst pair #&& nPred # psnd pair)
              (ptrace "evalTnAndAmount OK" $ pconstant True)
              (ptrace "predicate on TokenName/amount not satisfied" $
                pconstant False)
          pfst p = pfromData $ pfstBuiltin # p
          psnd p = pfromData $ psndBuiltin # p