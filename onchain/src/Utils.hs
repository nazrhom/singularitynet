module Utils(
    peq
    , pxor
    , plt
    , ple
    , pge
    , pgt
    , pfstData
    , psndData
    , oneOf
    , oneOfWith
) where

import Plutarch.Monadic qualified as P
import Plutarch.Api.V1 (PCurrencySymbol, PTokenName, PValue)

-- Term-level boolean functions
peq :: forall (s :: S) (a :: PType) . PEq a => Term s (a :--> a :--> PBool)
peq = phoistAcyclic $ plam $ \x y -> x #== y

pxor :: forall (s :: S) . Term s (PBool :--> PBool :--> PBool)
pxor = phoistAcyclic $ plam $ \x y -> pnot #$ pdata x #== pdata y

plt :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
plt = phoistAcyclic $ plam $ \lim x -> x #< lim

ple :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
ple = phoistAcyclic $ plam $ \lim x -> x #<= lim

pge :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
pge = plam $ \lim x -> pnot #$ pnot #$ x #< lim

pgt :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
pgt = plam $ \lim x -> pnot #$ pnot #$ x #<= lim

-- Convenient functions for accessing a pair's elements

-- | Access the first element of a `PBuiltinPair` and apply `pfromData`
pfstData ::
    forall (s :: S) (a :: PType) (b :: PType) .
    PIsData a => Term s (PBuiltinPair (PAsData a) b) -> Term s a
pfstData x = pfromData $ pfstBuiltin # x

-- | Access the second element of a `PBuiltinPair` and apply `pfromData`
psndData ::
    forall (s :: S) (a :: PType) (b :: PType) .
    PIsData b => Term s (PBuiltinPair a (PAsData b)) -> Term s b
psndData x = pfromData $ psndBuiltin # x

-- Functions for evaluating predicates on `PValue`s

-- | Returns `PTrue` if the token described by its `PCurrencySymbol` and
-- `PTokenName` is present only *once* in the `PValue`.
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

-- | Returns `PTrue` if only *one* token present in `PValue` satisfies *all* the
-- predicates given as parameters
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

-- | Assigns a boolean to each token in the value based on the the result of:
--
-- > csPred cs `pand` tnPred cs tn `pand` amountPred cs tn n
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
tokenPredicate ::
  forall (s :: S) .
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