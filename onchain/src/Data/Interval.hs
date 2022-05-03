module Data.Interval (
  PPeriodicInterval (..),
  pinterval,
  pintervalTo,
  pintervalFrom,
  pcontains,
  pperiodicContains,
) where

import Plutarch.Api.V1 (
  PClosure,
  PExtended (
    PFinite,
    PNegInf,
    PPosInf
  ),
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PTuple,
  PUpperBound (PUpperBound),
  ptuple,
 )

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Data.Natural (PNatural)
import Plutarch.Api.V1.Time (PPOSIXTime)
import Utils (ple, pletC, plt, pmatchC)

-- Functions for working with intervals

{- We need to define `POrd`-like functions for `PLowerBound` and `PUpperBound`.

   They might look cryptic, but the following facts can be observed from
   a truth-table. We have that:

   1. upperBound x _ <= upperBound x _ == ~(lowerBound x _ < lowerBound x _)
   1. upperBound x _ <  upperBound x _ == ~(lowerBound x _ <= lowerBound x _)

   where _ stands for any closure type (open or closed) and `x` is a given
   point. This is only true when the point `x` is fixed and *NOT* when the two
   endpoints have different finite valuees.

   This implementation is based on the one from `plutus-ledger-api`. It is a
   bit wasteful, since it pmatches twice the same arguments, but the alternative
   is way too verbose.
-}

-- This logic is common to all cases. If the endpoints are different and
-- finite, then one just needs to compare the inner type `a`. Otherwise, a
-- specific function needs to be called for each case.
compareEndpoints ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term
    s
    ( PExtended a
        :--> PClosure
        :--> PExtended a
        :--> PClosure
        :--> (a :--> a :--> PBool)
        :--> (PExtended a :--> PClosure :--> PExtended a :--> PClosure :--> PBool)
        :--> PBool
    )
compareEndpoints = phoistAcyclic $
  plam $
    \p1 c1 p2 c2 cmp caseLogic -> unTermCont $ do
      boolTup' <- unequalFiniteEndpoints p1 p2 cmp
      boolTup <- tcont $ pletFields @'["_0", "_1"] boolTup'
      equalAndFinite <- pletC $ pfromData boolTup._0
      cmpResult <- pletC $ pfromData boolTup._1
      pure $
        pif
          equalAndFinite
          (ptrace "equalAndFinite" cmpResult)
          $ (ptrace "caseLogic" $ caseLogic # p1 # c1 # p2 # c2)

-- Returns a tuple of booleans. If the first boolean is true, then the endpoints
-- are unequal and finite. The second gives gives the result of comparing the
-- endpoints with `cmp`.
unequalFiniteEndpoints ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term s (PExtended a) ->
  Term s (PExtended a) ->
  Term s (a :--> a :--> PBool) ->
  TermCont s (Term s (PTuple PBool PBool))
unequalFiniteEndpoints p1 p2 cmp = do
  pure $
    pmatch p1 $ \case
      PFinite x' -> pmatch p2 $ \case
        PFinite y' -> unTermCont $ do
          x <- pletC $ pfromData $ pfield @"_0" # x'
          y <- pletC $ pfromData $ pfield @"_0" # y'
          pure $ ptuple # pdata (pnot #$ x #== y) #$ pdata (cmp # y # x)
        _ -> ptuple # pconstantData False # pconstantData False
      _ -> ptuple # pconstantData False # pconstantData False

-- This function handles the case where the comparison operator is `<` and
-- the two endpoints are the lower bounds of the interval. It assumes that
-- `p1` == `p2` if both are `PFinite`.
lowerBoundLT ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term
    s
    ( PExtended a
        :--> PClosure
        :--> PExtended a
        :--> PClosure
        :--> PBool
    )
lowerBoundLT = phoistAcyclic $
  plam $ \p1 c1 p2 c2 ->
    ptrace "lowerBoundLT" $
      ptraceIfTrue "True" $
        pmatch p1 $ \case
          PNegInf _ -> pmatch p2 $ \case
            PNegInf _ -> pconstant False
            _ -> pconstant True
          PPosInf _ -> pconstant False
          PFinite n1' -> pmatch p2 $ \case
            PNegInf _ -> pconstant False
            PPosInf _ -> pconstant True
            PFinite n2' -> unTermCont $ do
              n1 <- pletC $ pfromData $ pfield @"_0" # n1'
              n2 <- pletC $ pfromData $ pfield @"_0" # n2'
              pure $ n1 #== n2 #&& c1 #&& pnot # c2

-- This function handles the case where the comparison operator is `<=` and
-- the two endpoints are the lower bounds of the interval. It assumes that
-- `p1` == `p2` if both are `PFinite`.
lowerBoundLE ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term
    s
    ( PExtended a
        :--> PClosure
        :--> PExtended a
        :--> PClosure
        :--> PBool
    )
lowerBoundLE = phoistAcyclic $
  plam $ \p1 c1 p2 c2 ->
    ptrace "lowerBoundLE" $
      ptraceIfTrue "True" $
        pmatch p1 $ \case
          PNegInf _ -> pconstant True
          PPosInf _ -> pmatch p2 $ \case
            PPosInf _ -> pconstant True
            _ -> pconstant False
          PFinite _ -> pmatch p2 $ \case
            PNegInf _ -> pconstant False
            PPosInf _ -> pconstant True
            PFinite _ -> unTermCont $ do
              pure $ c1 #|| pnot # c2

upperBoundLT ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term
    s
    ( PExtended a
        :--> PClosure
        :--> PExtended a
        :--> PClosure
        :--> PBool
    )
upperBoundLT = phoistAcyclic $
  plam $ \p1 c1 p2 c2 ->
    ptrace "upperBoundLT" $
      ptraceIfTrue "True" $
        pnot #$ lowerBoundLE # p1 # c1 # p2 # c2

upperBoundLE ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term
    s
    ( PExtended a
        :--> PClosure
        :--> PExtended a
        :--> PClosure
        :--> PBool
    )
upperBoundLE = phoistAcyclic $
  plam $ \p1 c1 p2 c2 ->
    ptrace "upperBoundLE" $
      ptraceIfTrue "True" $
        pnot #$ lowerBoundLT # p1 # c1 # p2 # c2

-- Now we define operations on `LowerBound` and `UpperBound` proper
(|<)
  , (|<=) ::
    forall (s :: S) (a :: PType).
    (PIsData a, PEq a, POrd a) =>
    Term s (PLowerBound a) ->
    Term s (PLowerBound a) ->
    Term s PBool
x' |< y' = unTermCont $ do
  x <- tcont $ pletFields @'["_0", "_1"] x'
  y <- tcont $ pletFields @'["_0", "_1"] y'
  pure $ compareEndpoints # x._0 # x._1 # y._0 # y._1 # plt # lowerBoundLT
x' |<= y' = unTermCont $ do
  x <- tcont $ pletFields @'["_0", "_1"] x'
  y <- tcont $ pletFields @'["_0", "_1"] y'
  pure $ compareEndpoints # x._0 # x._1 # y._0 # y._1 # ple # lowerBoundLE

(<|)
  , (<=|) ::
    forall (s :: S) (a :: PType).
    (PIsData a, PEq a, POrd a) =>
    Term s (PUpperBound a) ->
    Term s (PUpperBound a) ->
    Term s PBool
x' <| y' = unTermCont $ do
  x <- tcont $ pletFields @'["_0", "_1"] x'
  y <- tcont $ pletFields @'["_0", "_1"] y'
  pure $ compareEndpoints # x._0 # x._1 # y._0 # y._1 # plt # upperBoundLT
x' <=| y' = unTermCont $ do
  x <- tcont $ pletFields @'["_0", "_1"] x'
  y <- tcont $ pletFields @'["_0", "_1"] y'
  pure $ compareEndpoints # x._0 # x._1 # y._0 # y._1 # ple # upperBoundLE

-- | Returns true if the second interval is contained within the first
pcontains ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term s (PInterval a) ->
  Term s (PInterval a) ->
  Term s PBool
pcontains i1 i2 = unTermCont $ do
  i1F <- tcont $ pletFields @'["from", "to"] i1
  i2F <- tcont $ pletFields @'["from", "to"] i2
  pure $ i1F.from |<= i2F.from #&& i2F.to <=| i1F.to

{- | Build an interval out of two endpoints. The first endpoint is included
 but the last is *not*
-}
pinterval ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term s a ->
  Term s a ->
  Term s (PInterval a)
pinterval p1 p2 = pinterval' lBound hBound
  where
    lBound :: Term s (PLowerBound a)
    lBound = plowerBound p1 $ pconstant True
    hBound :: Term s (PUpperBound a)
    hBound = pupperBound p2 $ pconstant False

-- | Build an interval out of two bounds.
pinterval' ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term s (PLowerBound a) ->
  Term s (PUpperBound a) ->
  Term s (PInterval a)
pinterval' lBound hBound =
  pcon $
    PInterval $
      pdcons # pdata lBound
        #$ pdcons # pdata hBound # pdnil

-- | Build a closed-open interval from a given point to +inf.
pintervalFrom ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term s a ->
  Term s (PInterval a)
pintervalFrom p =
  pcon $
    PInterval $
      pdcons # lBound
        #$ pdcons # hBound # pdnil
  where
    hBound :: Term s (PAsData (PUpperBound a))
    hBound = pdata $ pposInf
    lBound :: Term s (PAsData (PLowerBound a))
    lBound = pdata $ plowerBound p $ pconstant True
    pposInf :: forall (s :: S) (a :: PType). Term s (PUpperBound a)
    pposInf =
      pcon $
        PUpperBound $
          pdcons # (pdata $ pcon $ PNegInf pdnil)
            #$ pdcons # pconstantData True # pdnil

-- | Build an open-open interval from -inf. to a given point
pintervalTo ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term s a ->
  Term s (PInterval a)
pintervalTo p =
  pcon $
    PInterval $
      pdcons # lBound
        #$ pdcons # hBound # pdnil
  where
    lBound :: Term s (PAsData (PLowerBound a))
    lBound = pdata $ pnegInf
    hBound :: Term s (PAsData (PUpperBound a))
    hBound = pdata $ pupperBound p $ pconstant False
    pnegInf :: forall (s :: S) (a :: PType). Term s (PLowerBound a)
    pnegInf =
      pcon $
        PLowerBound $
          pdcons # (pdata $ pcon $ PNegInf pdnil)
            #$ pdcons # pconstantData True # pdnil

-- | Build a lower bound, which can be either open or closed
plowerBound ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term s a ->
  Term s PBool ->
  Term s (PLowerBound a)
plowerBound p closed =
  pcon $
    PLowerBound $
      pdcons # pext p #$ pdcons # pdata closed # pdnil

-- | Build an upper bound, which can be either open or closed
pupperBound ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term s a ->
  Term s PBool ->
  Term s (PUpperBound a)
pupperBound p closed =
  pcon $
    PUpperBound $
      pdcons # pext p #$ pdcons # pdata closed # pdnil

{- | A datatype for periodic intervals. To make non-overlapping intervals easier
 to define, we introduce introduce `piStart` and `piEnd`. This allows
 the existence of many periodic intervals in the same segment from
 `piBaseOffset` + i*`piPeriod` to `piBaseOffset` + (i+1)*`piPeriod`
 The cycles field limits the amount of periods allowed.
-}
data PPeriodicInterval (s :: S) = PPeriodicInterval
  { piBaseOffset :: Term s PPOSIXTime
  , piPeriod :: Term s PPOSIXTime
  , piStartOffset :: Term s PPOSIXTime
  , piEndOffset :: Term s PPOSIXTime
  , piMaxCycles :: Term s PNatural
  }
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)

{- | A function that returns true if `i` is contained within the periodic
 interval `pi`.
-}
pperiodicContains ::
  forall (s :: S).
  Term
    s
    ( PPeriodicInterval :--> PInterval PPOSIXTime :--> PBool
    )
pperiodicContains = plam $ \pi i' -> unTermCont $ do
  ( PPeriodicInterval
      piBaseOffset'
      piStartOffset'
      piLength'
      piPeriod'
      maxCycles'
    ) <-
    pmatchC pi
  i <- tcont $ pletFields @'["from", "to"] i'
  iLowerBound <- tcont $ pletFields @'["_0", "_1"] i.from
  iUpperBound <- tcont $ pletFields @'["_0", "_1"] i.to
  iStart' <- getTime iLowerBound._0
  iEnd' <- getTime iUpperBound._0
  -- Convert all PPOSIXTimes to PIntegers
  let iStart = pto iStart'
      iEnd = pto iEnd'
      piBaseOffset = pto piBaseOffset'
      piStartOffset = pto piStartOffset'
      piLength = pto piLength'
      piPeriod = pto piPeriod'
      maxCycles = pto maxCycles'
  -- Calculate cycle number based on transaction's start
  cycleN <- pletC $ pquot # (iStart - piBaseOffset) # piPeriod
  -- Calculate start and end offset
  iStartOff <- pletC $ prem # (iStart - piBaseOffset) # piPeriod
  iEndOff <- pletC $ prem # (iEnd - piBaseOffset) # piPeriod
  pure $
    ptraceIfFalse
      "transaction range too wide"
      (iEnd - iStart #<= piLength)
      #&& ptraceIfFalse
        "cycle not within bounds"
        (0 #<= cycleN #&& cycleN #<= maxCycles)
      #&& ptraceIfFalse
        "transaction range starts too soon"
        (piStartOffset #<= iStartOff)
      #&& ptraceIfFalse
        "transaction range starts too late"
        (iEndOff #<= iEndOff)
  where
    getTime ::
      Term s (PExtended PPOSIXTime) -> TermCont s (Term s PPOSIXTime)
    getTime x = pure $
      pmatch x $ \case
        PFinite n -> pfield @"_0" # n
        _ -> ptraceError "error when getting PPOSIXTime from interval"

pext ::
  forall (s :: S) (a :: PType).
  PIsData a =>
  Term s a ->
  Term s (PAsData (PExtended a))
pext p = pdata $ pcon $ PFinite $ pdcons # pdata p # pdnil
