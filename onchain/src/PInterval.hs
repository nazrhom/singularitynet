{-# OPTIONS_GHC -Wno-orphans #-}

module PInterval (
  PPeriodicInterval (..),
  pinterval,
  pintervalTo,
  pintervalFrom,
  pcontains,
  pperiodicContains,
  getBondedPeriod,
) where

import Plutarch.Api.V1 (
  PExtended (
    PFinite,
    PNegInf,
    PPosInf
  ),
  PInterval (PInterval),
  PLowerBound (PLowerBound),
  PPOSIXTimeRange,
  PUpperBound (PUpperBound),
 )

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))
import PNatural (PNatural)
import PTypes (
  PBondedPoolParams,
  PPeriod,
  bondingPeriod,
  closingPeriod,
  depositWithdrawPeriod,
  onlyWithdrawPeriod,
  unavailablePeriod,
 )
import Plutarch (PlutusType (pcon', pmatch'))
import Plutarch.Api.V1.Time (PPOSIXTime)
import Plutarch.Unsafe (punsafeCoerce)
import Utils (pfalse, pletC, pletDataC, pmatchC, pnestedIf, ptrue, (>:))

-- We create a `POrdering` type to simplify the implementation of the functions
-- Internally it is represented as a `PInteger`s from 0 to 2
data POrdering (s :: S) = PLT | PEQ | PGT
  deriving stock (Eq)

instance PlutusType POrdering where
  type PInner POrdering _ = PInteger
  pcon' PLT = pconstant 0
  pcon' PEQ = pconstant 1
  pcon' PGT = pconstant 2
  pmatch' x f =
    pif
      (x #== 0)
      (f PLT)
      ( pif
          (x #== 1)
          (f PEQ)
          ( pif
              (x #== 2)
              (f PGT)
              (ptraceError "POrdering: unexpected error when matching value")
          )
      )

instance PEq POrdering where
  o1 #== o2 = pto o1 #== pto o2

instance POrd POrdering where
  o1 #<= o2 = pto o1 #<= pto o2
  o1 #< o2 = pto o1 #< pto o2

pcompare ::
  forall (a :: PType) (s :: S). POrd a => Term s (a :--> a :--> POrdering)
pcompare = phoistAcyclic $
  plam $ \x y ->
    pif
      (x #< y)
      (pcon PLT)
      ( pif
          ((x #<= y) #&& (pnot # (x #< y))) -- "local eq instance"
          (pcon PEQ)
          (pcon PGT)
      )

-- We define `POrd` instances for `PUpperBound` and `PLowerBound`

instance (POrd a, PIsData a) => POrd (PLowerBound a) where
  lb0 #<= lb1 = leq # lb0 # lb1
    where
      leq = phoistAcyclic $
        plam $ \x y -> unTermCont $ do
          x' <- tcont $ pletFields @'["_0", "_1"] x
          y' <- tcont $ pletFields @'["_0", "_1"] y
          fst' <- pmatchC $ pcompare @(PExtended a) # x'._0 # y'._0
          pure $ case fst' of
            -- An open lower bound is bigger than a closed lower bound.
            PEQ -> pnot # y'._1 #|| x'._1
            PLT -> pcon PTrue
            PGT -> pcon PFalse

  lb0 #< lb1 = lt # lb0 # lb1
    where
      lt = phoistAcyclic $
        plam $ \x y -> unTermCont $ do
          x' <- tcont $ pletFields @'["_0", "_1"] x
          y' <- tcont $ pletFields @'["_0", "_1"] y
          fst' <- pmatchC $ pcompare @(PExtended a) # x'._0 # y'._0
          pure $ case fst' of
            -- An open lower bound is bigger than a closed lower bound. This corresponds
            -- to the *reverse* of the normal order on Bool.
            PEQ -> pnot # y'._1 #&& x'._1
            PLT -> pcon PTrue
            PGT -> pcon PFalse

instance (PIsData a, POrd a) => POrd (PUpperBound a) where
  ub0 #<= ub1 = leq # ub0 # ub1
    where
      leq = phoistAcyclic $
        plam $ \x y -> unTermCont $ do
          xF <- tcont $ pletFields @'["_0", "_1"] x
          yF <- tcont $ pletFields @'["_0", "_1"] y
          fst' <- pmatchC $ pcompare @(PExtended a) # xF._0 # yF._0
          pure $ case fst' of
            -- A closed upper bound is bigger than an open upper bound
            -- If x == y, then either x is open or y is closed
            PEQ -> pnot # xF._1 #|| yF._1
            PLT -> pcon PTrue
            PGT -> pcon PFalse

  ub0 #< ub1 = lt # ub0 # ub1
    where
      lt = phoistAcyclic $
        plam $ \x y -> unTermCont $ do
          x' <- tcont $ pletFields @'["_0", "_1"] x
          y' <- tcont $ pletFields @'["_0", "_1"] y
          fst' <- pmatchC $ pcompare @(PExtended a) # x'._0 # y'._0
          pure $ case fst' of
            -- A closed upper bound is bigger than an open upper bound.
            -- If x == y, then x must be open and y must be closed
            PEQ -> pnot # x'._1 #&& y'._1
            PLT -> pcon PTrue
            PGT -> pcon PFalse

instance (POrd a, PIsData a) => POrd (PExtended a) where
  ex0 #<= ex1 = leq # ex0 # ex1
    where
      leq = phoistAcyclic $
        plam $ \x' y' -> unTermCont $ do
          x <- pmatchC x'
          y <- pmatchC y'
          pure $ case (x, y) of
            (PNegInf _, _) -> pcon PTrue
            (_, PNegInf _) -> pcon PFalse
            (_, PPosInf _) -> pcon PTrue
            (PPosInf _, _) -> pcon PFalse
            (PFinite a, PFinite b) ->
              (pfield @"_0" # a :: Term _ a) #<= pfield @"_0" # b

  ex0 #< ex1 = lt # ex0 # ex1
    where
      lt = phoistAcyclic $
        plam $ \x' y' -> unTermCont $ do
          x <- pmatchC x'
          y <- pmatchC y'
          pure $ case (x, y) of
            (_, PNegInf _) -> pcon PFalse
            (PNegInf _, _) -> pcon PTrue
            (PPosInf _, _) -> pcon PFalse
            (_, PPosInf _) -> pcon PTrue
            (PFinite a, PFinite b) ->
              (pfield @"_0" # a :: Term _ a) #< pfield @"_0" # b

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
  pure $
    (i1F.from :: Term s (PLowerBound a)) #<= i2F.from
      #&& (i2F.to :: Term s (PUpperBound a)) #<= i1F.to

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
 interval `pi`. This function fails if it receives a non-finite interval
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
        _ -> ptraceError
          "pperiodicContains: received a non-finite interval"

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
          , pintervalFrom closeStart `pcontains` txTimeRange
              >: closingPeriod
          ]
          $ ptraceError
            "the transaction's range does not belong to any valid period"
