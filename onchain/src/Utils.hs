module Utils (
  peq,
  pxor,
  plt,
  ple,
  pge,
  pgt,
  ptrue,
  pfalse,
  pnestedIf,
  pfind,
  pfstData,
  psndData,
  ptraceBool,
  oneOf,
  oneOfWith,
  pletC,
  pletDataC,
  pmatchC,
  pconstantC,
  guardC,
  ptryFromData,
  ptryFromUndata,
  getCs,
  getInput,
  getDatum,
  getDatumHash,
  getContinuingOutputWithNFT,
  toPBool,
  (>:),
) where

import PTypes (PAssetClass)
import Plutarch.Api.V1 (
  PAddress,
  PCurrencySymbol,
  PDatum,
  PDatumHash,
  PMaybeData (PDJust, PDNothing),
  PScriptPurpose (PMinting, PSpending),
  PTokenName,
  PTuple,
  PValue,
 )
import Plutarch.Api.V1.Tx (PTxInInfo, PTxOut, PTxOutRef)
import Plutarch.Lift (
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Monadic qualified as P
import Plutarch.TryFrom (PTryFrom, ptryFrom)

import UnbondedStaking.PTypes (PBoolData (PDFalse, PDTrue))

-- Term-level boolean functions
peq :: forall (s :: S) (a :: PType). PEq a => Term s (a :--> a :--> PBool)
peq = phoistAcyclic $ plam $ \x y -> x #== y

pxor :: forall (s :: S). Term s (PBool :--> PBool :--> PBool)
pxor = phoistAcyclic $ plam $ \x y -> pnot #$ pdata x #== pdata y

plt ::
  forall (s :: S) (a :: PType).
  POrd a =>
  Term s (a :--> a :--> PBool)
plt = phoistAcyclic $ plam $ \lim x -> x #< lim

ple ::
  forall (s :: S) (a :: PType).
  POrd a =>
  Term s (a :--> a :--> PBool)
ple = phoistAcyclic $ plam $ \lim x -> x #<= lim

pge ::
  forall (s :: S) (a :: PType).
  POrd a =>
  Term s (a :--> a :--> PBool)
pge = phoistAcyclic $ plam $ \lim x -> pnot #$ x #< lim

pgt ::
  forall (s :: S) (a :: PType).
  POrd a =>
  Term s (a :--> a :--> PBool)
pgt = phoistAcyclic $ plam $ \lim x -> pnot #$ x #<= lim

ptrue :: forall (s :: S). Term s PBool
ptrue = pconstant True

pfalse :: forall (s :: S). Term s PBool
pfalse = pconstant False

-- Functions for checking conditions in nested structures

{- | Build nested conditions. It takes an association list of conditions and
 and results. It evaluates the conditions in order: whenever a condition
 is satisfied, its associated result is returned.

 Analogous to a nested `pif` structure.
-}
pnestedIf ::
  forall (s :: S) (a :: PType).
  [(Term s PBool, Term s a)] ->
  Term s a ->
  Term s a
pnestedIf [] def = def
pnestedIf ((cond, x) : conds) def = pif cond x $ pnestedIf conds def

-- | A pair builder useful for avoiding parentheses
infixr 1 >:

(>:) :: forall (a :: Type) (b :: Type). a -> b -> (a, b)
(>:) = (,)

-- Convenient functions for accessing a pair's elements

-- | Access the first element of a `PBuiltinPair` and apply `pfromData`
pfstData ::
  forall (s :: S) (a :: PType) (b :: PType).
  PIsData a =>
  Term s (PBuiltinPair (PAsData a) b) ->
  Term s a
pfstData x = pfromData $ pfstBuiltin # x

-- | Access the second element of a `PBuiltinPair` and apply `pfromData`
psndData ::
  forall (s :: S) (a :: PType) (b :: PType).
  PIsData b =>
  Term s (PBuiltinPair a (PAsData b)) ->
  Term s b
psndData x = pfromData $ psndBuiltin # x

-- Functions for lists

{- | Returns the first element that matches the predicate. If no element matches
 the predicate, it throws an error
-}
pfind ::
  forall (s :: S) (a :: PType).
  PIsData a =>
  Term s (a :--> PBool) ->
  Term s (PBuiltinList (PAsData a)) ->
  TermCont s (Term s a)
pfind pred ls = pure $ (pfix # plam go) # ls
  where
    go ::
      Term s (PBuiltinList (PAsData a) :--> a) ->
      Term s (PBuiltinList (PAsData a)) ->
      Term s a
    go self ls = pmatch ls $ \case
      PNil -> ptraceError "pfind: could not find element in list"
      PCons x xs -> plet (pfromData x) $ \x' -> pif (pred # x') x' (self # xs)

-- Functions for debugging

-- | Print one message or the other depending on the boolean condition.
ptraceBool ::
  forall (s :: S).
  Term s PString ->
  -- |^ The common message
  Term s PString ->
  -- |^ The `ptrue` part
  Term s PString ->
  -- |^ The `pfalse` part
  Term s PBool ->
  Term s PBool
ptraceBool common trueMsg falseMsg cond = ptrace msg cond
  where
    msg :: Term s PString
    msg = pif cond (common <> " " <> trueMsg) (common <> " " <> falseMsg)

-- Functions for evaluating predicates on `PValue`s

{- | Returns `PTrue` if the token described by its `PCurrencySymbol` and
 `PTokenName` is present only *once* in the `PValue`.
-}
oneOf ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol :--> PTokenName :--> PValue :--> PBool
    )
oneOf = phoistAcyclic $
  plam $ \cs tn val ->
    oneOfWith
      # (peq # cs)
      # (peq # tn)
      # (ple # 1)
      #$ val

{- | Returns `PTrue` if only *one* token present in `PValue` satisfies *all* the
 predicates given as parameters
-}
oneOfWith ::
  forall (s :: S).
  Term
    s
    ( (PCurrencySymbol :--> PBool)
        :--> (PTokenName :--> PBool)
        :--> (PInteger :--> PBool)
        :--> PValue
        :--> PBool
    )
oneOfWith = phoistAcyclic $
  plam $ \csPred tnPred nPred ->
    tokenPredicate pxor csPred tnPred nPred

{- | Assigns a boolean to each token in the value based on the the result of:

 > csPred cs `pand` tnPred cs tn `pand` amountPred cs tn n

 where each token is a tuple `(cs, tn, n)`.

 Then, all the booleans are combined according to the boolean operator `op`.

 This allows short-circuiting evaluation (e.g: a failure in `csPred`
 avoids evaluating the remaining predicates). `op` can be any binary boolean
 operator, like `pxor` (if only one token needs to satisfy all predicates) or
 `pand` (if all tokens must satisfy the predicates).

 However, this generic function does not allow short-circuiting row-wise,
 meaning the `op` is strict in both arguments.
-}
tokenPredicate ::
  forall (s :: S).
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
    matchPair ::
      forall (a :: PType).
      PLift a =>
      Term s (PBuiltinList a) ->
      (Term s a -> Term s PBool) ->
      Term s PBool
    matchPair ls cont = (pfix # plam go) # ls # plam cont
      where
        -- Recurring function for the Y combinator
        go ::
          forall (a :: PType).
          PLift a =>
          Term s (PBuiltinList a :--> (a :--> PBool) :--> PBool) ->
          Term s (PBuiltinList a) ->
          Term s (a :--> PBool) ->
          Term s PBool
        go self ls cont = pmatch ls $ \case
          PNil -> pconstant False
          PCons p ps -> boolOp # (cont # p) #$ self # ps # cont
    evalCs ::
      forall (a :: PType).
      PIsData a =>
      Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData a)) ->
      (Term s a -> Term s PBool) ->
      (Term s PBool)
    evalCs pair cont =
      pif
        (csPred # pfstData pair)
        (ptrace "evalCs OK" $ cont $ psndData pair)
        ( ptrace "predicate on CurrencySymbol not satisfied" $
            pconstant False
        )
    evalTnAndAmount ::
      Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) ->
      Term s PBool
    evalTnAndAmount pair =
      pif
        (tnPred # pfstData pair #&& nPred # psndData pair)
        (ptrace "evalTnAndAmount OK" $ pconstant True)
        ( ptrace "predicate on TokenName/amount not satisfied" $
            pconstant False
        )

-- Functions for working with `TermCont`

-- | Makes `a` constant and wraps it in a `TermCont`
pconstantC ::
  forall (s :: S) (a :: PType).
  PUnsafeLiftDecl a =>
  PLifted a ->
  TermCont s (Term s a)
pconstantC = pure . pconstant

-- | `pmatch` for the `TermCont` monad
pmatchC ::
  forall (s :: S) (a :: PType).
  PlutusType a =>
  Term s a ->
  TermCont s (a s)
pmatchC = tcont . pmatch

-- | `plet` for the `TermCont` monad
pletC :: forall (s :: S) (a :: PType). Term s a -> TermCont s (Term s a)
pletC = tcont . plet

-- | Converts from `Data` and binds result with `pletC`
pletDataC ::
  forall (s :: S) (a :: PType).
  PIsData a =>
  Term s (PAsData a) ->
  TermCont s (Term s a)
pletDataC = pletC . pfromData

-- | Boolean guard for the `TermCont` monad
guardC ::
  forall (s :: S).
  Term s PString ->
  Term s PBool ->
  TermCont s (Term s PUnit)
guardC errMsg cond = pure $ pif cond (pconstant ()) $ ptraceError errMsg

-- Functions for working with the `PTryFrom` class

-- | Copied from plutarch-extra
ptryFromData ::
  forall (a :: PType) (s :: S).
  PTryFrom PData (PAsData a) =>
  Term s PData ->
  TermCont s (Term s (PAsData a))
ptryFromData x = fst <$> tcont (ptryFrom @(PAsData a) x)

ptryFromUndata ::
  forall (a :: PType) (s :: S).
  (PIsData a, PTryFrom PData (PAsData a)) =>
  Term s PData ->
  TermCont s (Term s a)
ptryFromUndata x'' = do
  x' <- ptryFromData @a x''
  pure $ pfromData x'

-- Helper functions for retrieving data in a validator

-- | Gets the currency symbol of the script (equivalent to ownCurrencySymbol)
getCs ::
  forall (s :: S).
  Term s PScriptPurpose ->
  TermCont s (Term s PCurrencySymbol)
getCs purpose = pure $
  pmatch purpose $ \case
    PMinting cs -> pfield @"_0" # cs
    _ -> ptraceError "getCs: not a minting transaction"

{- | Gets the input being spent. If not available, it will fail with
 an error.
-}
getInput ::
  forall (s :: S).
  Term s PScriptPurpose ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PTxInInfo)
getInput purpose txInInfos = pure $ getInput' # purpose # txInInfos
  where
    getInput' ::
      forall (s :: S).
      Term
        s
        ( PScriptPurpose
            :--> PBuiltinList (PAsData PTxInInfo)
            :--> PTxInInfo
        )
    getInput' = phoistAcyclic $
      plam $ \purpose txInInfos -> unTermCont $ do
        let inputOutRef = getSpendingRef purpose
        pfind (predicate # inputOutRef) txInInfos
    predicate ::
      forall (s :: S).
      Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    predicate = phoistAcyclic $
      plam $ \inputOutRef txInInfo ->
        pdata inputOutRef #== pdata (pfield @"outRef" # txInInfo)
    getSpendingRef ::
      forall (s :: S).
      Term s PScriptPurpose ->
      Term s PTxOutRef
    getSpendingRef = flip pmatch $ \case
      PSpending outRef -> pfield @"_0" # outRef
      _ ->
        ptraceError
          "getInput: cannot get input because tx is not of \
          \spending type"

{- | Gets the continuing output that shares the same address and contains the
 the given NFT. If no such output exists, it will fail with an error.
-}
getContinuingOutputWithNFT ::
  forall (s :: S).
  Term s PAddress ->
  Term s PAssetClass ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  TermCont s (Term s PTxOut)
getContinuingOutputWithNFT addr ac outputs =
  pure $ getContinuingOutputWithNFT' # addr # ac # outputs
  where
    getContinuingOutputWithNFT' ::
      forall (s :: S).
      Term
        s
        ( PAddress
            :--> PAssetClass
            :--> PBuiltinList (PAsData PTxOut)
            :--> PTxOut
        )
    getContinuingOutputWithNFT' = phoistAcyclic $
      plam $ \addr ac outputs ->
        unTermCont $
          pfind
            (sameAddrAndNFT addr ac)
            outputs
    sameAddrAndNFT ::
      forall (s :: S).
      Term s PAddress ->
      Term s PAssetClass ->
      Term s (PTxOut :--> PBool)
    sameAddrAndNFT addr ac = plam $ \output -> unTermCont $ do
      outputF <- tcont $ pletFields @'["address", "value"] output
      acF <- tcont $ pletFields @'["currencySymbol", "tokenName"] ac
      pure $
        pdata outputF.address #== pdata addr
          #&& oneOf # acF.currencySymbol # acF.tokenName # outputF.value

{- | Gets the `DatumHash` from a `PTxOut`. If not available, it will fail with
 an error.
-}
getDatumHash ::
  forall (s :: S).
  Term s PTxOut ->
  TermCont s (Term s PDatumHash)
getDatumHash txOut = pure $ getDatumHash' # txOut
  where
    getDatumHash' :: forall (s :: S). Term s (PTxOut :--> PDatumHash)
    getDatumHash' = phoistAcyclic $
      plam $ \txOut ->
        pmatch (pfield @"datumHash" # txOut) $ \case
          PDJust datumHash' -> pfield @"_0" # datumHash'
          PDNothing _ ->
            ptraceError
              "getDatumHash: could not find datum hash \
              \in txOut"

{- | Gets `Datum` by its `DatumHash`. If not available, it will fail with an
 error.
-}
getDatum ::
  forall (s :: S).
  Term s PDatumHash ->
  Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum))) ->
  TermCont s (Term s PDatum)
getDatum datHash dats = pure $ getDatum' # datHash # dats
  where
    getDatum' ::
      forall (s :: S).
      Term
        s
        ( PDatumHash
            :--> ( PBuiltinList (PAsData (PTuple PDatumHash PDatum))
                    :--> PDatum
                 )
        )
    getDatum' = phoistAcyclic $
      plam $ \datHash dats -> unTermCont $ do
        datHashAndDat <- pfind (checkHash # datHash) dats
        pure $ pfield @"_1" # datHashAndDat
    checkHash ::
      forall (s :: S).
      Term s (PDatumHash :--> PTuple PDatumHash PDatum :--> PBool)
    checkHash = phoistAcyclic $
      plam $ \datHash tup ->
        pfield @"_0" # tup #== datHash

toPBool :: forall (s :: S). Term s (PBoolData :--> PBool)
toPBool =
  plam $ \pbd ->
    pmatch pbd $ \case
      PDFalse _ -> pconstant False
      PDTrue _ -> pconstant True
