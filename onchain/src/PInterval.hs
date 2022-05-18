module PInterval (
  PPeriodicInterval (..),
  pinterval,
  pintervalTo,
  pintervalFrom,
  pcontains,
  pperiodicContains,
  lowerBoundLt,
  lowerBoundLe,
  getBondedPeriod,
  (|<),
  (<=|),
  (|<=),
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
  PPOSIXTimeRange,
  PTuple,
  PUpperBound (PUpperBound),
  ptuple,
 )

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import PNatural (PNatural)
import PTypes (PBondedPoolParams, PPeriod (BondingPeriod, ClosingPeriod, DepositWithdrawPeriod, OnlyWithdrawPeriod, UnavailablePeriod), depositWithdrawPeriod, bondingPeriod, unavailablePeriod, onlyWithdrawPeriod, closingPeriod)
import Plutarch.Api.V1.Time (PPOSIXTime)
import Plutarch.Unsafe (punsafeCoerce)
import Utils (pfalse, ple, pletC, pletDataC, plt, pmatchC, pnestedIf, ptrue, (>:))

-- Functions for working with intervals

{- We need to define `POrd`-like functions for `PLowerBound` and `PUpperBound`.

   They might look cryptic, but the following facts can be observed from
   a truth-table. We have that:

   1. upperBound x _ <= upperBound x _ == ~(lowerBound x _ < lowerBound x _)
   1. upperBound x _ <  upperBound x _ == ~(lowerBound x _ <= lowerBound x _)

   where _ stands for any closure type (open or closed) and `x` is a given
   point. This is only true when the point `x` is fixed and *NOT* when the two
   endpoints have different finite values.

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
      unequalAndFinite <- pletC $ pfromData boolTup._0
      cmpResult <- pletC $ pfromData boolTup._1
      pure $
        pif
          (ptraceIfTrue "unequalAndFinite" unequalAndFinite)
          (ptraceIfTrue "cmpResult" cmpResult)
          $ caseLogic # p1 # c1 # p2 # c2

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
unequalFiniteEndpoints p1 p2 cmp =
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
lowerBoundLt ::
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
lowerBoundLt = phoistAcyclic $
  plam $ \p1 c1 p2 c2 ->
    pmatch p1 $ \case
      PNegInf _ -> pmatch p2 $ \case
        PNegInf _ -> pfalse
        _ -> ptrue
      PPosInf _ -> pfalse
      PFinite n1' -> pmatch p2 $ \case
        PNegInf _ -> pfalse
        PPosInf _ -> ptrue
        PFinite n2' -> unTermCont $ do
          n1 <- pletC $ pfromData $ pfield @"_0" # n1'
          n2 <- pletC $ pfromData $ pfield @"_0" # n2'
          pure $ n1 #== n2 #&& c1 #&& pnot # c2

-- This function handles the case where the comparison operator is `<=` and
-- the two endpoints are the lower bounds of the interval. It assumes that
-- `p1` == `p2` if both are `PFinite`.
lowerBoundLe ::
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
lowerBoundLe = phoistAcyclic $
  plam $ \p1 c1 p2 c2 ->
    pmatch p1 $ \case
      PNegInf _ -> ptrue
      PPosInf _ -> pmatch p2 $ \case
        PPosInf _ -> ptrue
        _ -> pfalse
      PFinite _ -> pmatch p2 $ \case
        PNegInf _ -> pfalse
        PPosInf _ -> ptrue
        PFinite _ -> c1 #|| pnot # c2

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
  pure $ compareEndpoints # x._0 # x._1 # y._0 # y._1 # plt # lowerBoundLt
x' |<= y' = unTermCont $ do
  x <- tcont $ pletFields @'["_0", "_1"] x'
  y <- tcont $ pletFields @'["_0", "_1"] y'
  pure $ compareEndpoints # x._0 # x._1 # y._0 # y._1 # ple # lowerBoundLe

(<=|) ::
    forall (s :: S) (a :: PType).
    (PIsData a, PEq a, POrd a) =>
    Term s (PUpperBound a) ->
    Term s (PUpperBound a) ->
    Term s PBool
x' <=| y' = unTermCont $ do
  x <- tcont $ pletFields @'["_0", "_1"] x'
  y <- tcont $ pletFields @'["_0", "_1"] y'
  pure $ compareEndpoints # x._0 # x._1 # y._0 # y._1 # ple # lowerBoundLt

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
  pure $ ptraceIfFalse "1" (i1F.from |<= i2F.from)
        #&& ptraceIfFalse "2" (i2F.to <=| i1F.to)

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
    lBound = plowerBound p1 ptrue
    hBound :: Term s (PUpperBound a)
    hBound = pupperBound p2 pfalse

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
pintervalFrom p = pinterval' lBound pposInf
  where
    lBound :: Term s (PLowerBound a)
    lBound = plowerBound p ptrue
    pposInf :: forall (s :: S) (a :: PType). Term s (PUpperBound a)
    pposInf =
      pcon $
        PUpperBound $
          pdcons # (pdata $ pcon $ PPosInf pdnil)
            #$ pdcons # pconstantData True # pdnil

-- | Build an open-open interval from -inf. to a given point
pintervalTo ::
  forall (s :: S) (a :: PType).
  (PIsData a, PEq a, POrd a) =>
  Term s a ->
  Term s (PInterval a)
pintervalTo p = pinterval' pnegInf hBound
  where
    hBound :: Term s (PUpperBound a)
    hBound = pupperBound p $ pfalse
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
 to define, we introduce `piStart` and `piEnd`. This allows
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
  -- Get fields from periodic interval
  ( PPeriodicInterval
      piBaseOffset'
      piPeriod'
      piStartOffset'
      piEndOffset'
      maxCycles'
    ) <-
    pmatchC pi
  -- Get fields from TX range
  i <- tcont $ pletFields @'["from", "to"] i'
  iLowerBound <- tcont $ pletFields @'["_0", "_1"] i.from
  iUpperBound <- tcont $ pletFields @'["_0", "_1"] i.to
  iStart' <- getTime iLowerBound._0
  iEnd' <- getTime iUpperBound._0
  -- Convert all PPOSIXTimes to PIntegers
  let iStart = pto iStart'
      iEnd = pto iEnd'
      piBaseOffset = pto piBaseOffset'
      piPeriod = pto piPeriod'
      piStartOffset = pto piStartOffset'
      piEndOffset = pto piEndOffset'
      maxCycles = pto maxCycles'
  -- Calculate cycle number based on transaction's start
  cycleN <- pletC $ pquot # (iStart - piBaseOffset) # piPeriod
  -- Calculate start and end of period
  let piStart = piBaseOffset + cycleN * piPeriod + piStartOffset
      piEnd = piBaseOffset + cycleN * piPeriod + piEndOffset
  pure $
    ptraceIfFalse
      "pperiodicContains: transaction range too wide"
      (iEnd - iStart #<= piEndOffset - piStartOffset)
      #&& ptraceIfFalse
        "pperiodicContains: cycle not within bounds"
        (0 #<= cycleN #&& cycleN #< maxCycles)
      #&& ptraceIfFalse
        "pperiodicContains: transaction range starts too soon"
        (piStart #<= iStart)
      #&& ptraceIfFalse
        "pperiodicContains: transaction range ends too late"
        (iEnd #<= piEnd)
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

-- | Get the BondedPool's period a certain POSIXTimeRange belongs to
getBondedPeriod ::
  Term
    s
    ( PPOSIXTimeRange
        :--> PBondedPoolParams
        :--> PPeriod
    )
getBondedPeriod = phoistAcyclic $
  plam $
    \txTimeRange params ->
      getPeriod' txTimeRange params
  where
    getPeriod' ::
      forall (s :: S).
      Term s PPOSIXTimeRange ->
      Term s PBondedPoolParams ->
      Term s PPeriod
    getPeriod' txTimeRange params = unTermCont $ do
      -- Retrieve fields
      paramsF <-
        tcont $
          pletFields
            @'["iterations", "start", "end", "userLength", "bondingLength"]
            params
      -- Convert from data
      iterations <- pletDataC paramsF.iterations
      start <- pletDataC paramsF.start
      end <- pletDataC paramsF.end
      userLength <- pletDataC paramsF.userLength
      bondingLength <- pletDataC paramsF.bondingLength
      period <- pletC $ punsafeCoerce $ pto userLength + pto bondingLength
      let -- We define the periodic intervals in which the Deposit/Withdrawal
          -- and Bonding will happen
          depositWithdrawal =
            PPeriodicInterval
              { piBaseOffset = start
              , piPeriod = period
              , piStartOffset = pconstant 0
              , piEndOffset = userLength
              , piMaxCycles = iterations
              }
          bonding =
            depositWithdrawal
              { piStartOffset = userLength
              , piEndOffset = punsafeCoerce $ pto userLength + pto bondingLength
              }
          -- Calculate closing period's start time
          closeStart :: Term s PPOSIXTime
          closeStart = punsafeCoerce $ pto end + pto userLength
      pure $
        pnestedIf
          [ pintervalTo start `pcontains` txTimeRange
              >: unavailablePeriod
          , pperiodicContains # pcon depositWithdrawal # txTimeRange
              >: depositWithdrawPeriod 
          , pperiodicContains # pcon bonding # txTimeRange
              >: bondingPeriod
          , pinterval end closeStart `pcontains` txTimeRange
              >: onlyWithdrawPeriod 
          , ptrace "Closing..." (pintervalFrom closeStart `pcontains` txTimeRange)
              >: closingPeriod
          ]
          $ ptraceError
            "the transaction's range does not belong to any valid period"
