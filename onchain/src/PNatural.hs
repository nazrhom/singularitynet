{-# LANGUAGE UndecidableInstances #-}
{- This module contains orphan instances. This is because of one reason:

    * The module `SingularityNet.Types` needs to be compiled separately in the
      `common` package, which does not have Plutarch. Because of this, the
      `PConstant` instance for `Natural` needs to be defined here instead
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PNatural (
  PNatural (..),
  PNatRatio (..),
  PNonNegative (..),
  mkNatRatioUnsafe,
  mkNatUnsafe,
  natPow,
  natOne,
  natZero,
  ratZero,
  toNatRatio,
  roundUp,
  roundDown,
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import SingularityNet.Natural (
  NatRatio (NatRatio),
  Natural (Natural),
 )

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (
  PConstant (PConstantRepr, PConstanted, pconstantFromRepr, pconstantToRepr),
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.TryFrom (PTryFrom)

{- | `Natural` synonym.
 It wraps a `PInteger`. It derives all of its instances
 from it, with the exception of `PIntegral`, since its methods can produce
 non-positive results.
-}
newtype PNatural (s :: S) = PNatural (Term s PInteger)
  deriving stock (GHC.Generic)
  deriving
    (PEq, POrd, PIsData, PlutusType)
    via (DerivePNewtype PNatural PInteger)

deriving via
  DerivePNewtype (PAsData PNatural) (PAsData PInteger)
  instance
    PTryFrom PData (PAsData PNatural)

instance PUnsafeLiftDecl PNatural where
  type PLifted PNatural = Natural

instance PConstant Natural where
  type PConstantRepr Natural = Integer
  type PConstanted Natural = PNatural
  pconstantToRepr (Natural n) = fromIntegral n
  pconstantFromRepr = gt0

-- | A natural datatype that wraps a pair of Naturals
newtype PNatRatio (s :: S)
  = PNatRatio
      ( Term
          s
          ( PDataRecord
              '[ "numerator" ':= PNatural
               , "denominator" ':= PNatural
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PNatRatio

deriving via
  PAsData (PIsDataReprInstances PNatRatio)
  instance
    PTryFrom PData (PAsData PNatRatio)

instance PUnsafeLiftDecl PNatRatio where
  type PLifted PNatRatio = NatRatio

deriving via
  (DerivePConstantViaData NatRatio PNatRatio)
  instance
    (PConstant NatRatio)

-- We have to define `PEq` and `POrd` instances manually
instance PEq PNatRatio where
  -- We can assume that fractions are normalized because constant
  -- values are generated from `Ratio`s and all arithmetic operations also
  -- normalize their results
  a #== b = pdata a #== pdata b

instance POrd PNatRatio where
  a #<= b = unTermCont $ do
    (n1, d1) <- getIntegers a
    (n2, d2) <- getIntegers b
    pure $ n1 * d2 #<= n2 * d1
  a #< b = unTermCont $ do
    (n1, d1) <- getIntegers a
    (n2, d2) <- getIntegers b
    pure $ n1 * d2 #< n2 * d1

-- | The same as `NonNegative` but for plutarch types
class PNonNegative (a :: PType) where
  (#+) :: forall (s :: S). Term s a -> Term s a -> Term s a
  (#*) :: forall (s :: S). Term s a -> Term s a -> Term s a
  (#-) :: forall (s :: S). Term s a -> Term s a -> Term s (PMaybe a)

instance PNonNegative PNatural where
  x #+ y = pcon . PNatural $ pto x + pto y
  x #* y = pcon . PNatural $ pto x * pto y
  x #- y = unTermCont $ do
    let x' = pto x
        y' = pto y
    diff <- pletC $ x' - y'
    pure $
      pif
        (diff #< 0)
        (pcon PNothing)
        (pcon . PJust $ pcon . PNatural $ diff)

instance PNonNegative PNatRatio where
  x #+ y = unTermCont $ do
    (nx, dx) <- getIntegers x
    (ny, dy) <- getIntegers y
    n' <- pletC $ nx * dy + ny * dx
    d' <- pletC $ dx * dy
    let commonD = pgcd n' d'
    pure $
      mkNatRatioUnsafe
        (pdiv # n' # commonD)
        (pdiv # d' # commonD)
  x #* y = unTermCont $ do
    (nx, dx) <- getIntegers x
    (ny, dy) <- getIntegers y
    pure $ mkNatRatioUnsafe (nx * ny) (dx * dy)

  x #- y = unTermCont $ do
    (nx, dx) <- getIntegers x
    (ny, dy) <- getIntegers y
    n' <- pletC $ nx * dy - ny * dx
    d' <- pletC $ dx * dy
    let commonD = pgcd n' d'
    pure $
      pif
        (n' #< 0)
        (pcon PNothing)
        $ pcon . PJust $
          mkNatRatioUnsafe
            (pdiv # n' # commonD)
            (pdiv # d' # commonD)

-- Conversion functions

toNatRatio :: forall (s :: S). Term s PNatural -> Term s PNatRatio
toNatRatio n = mkNatRatioUnsafe (pto n) 1

roundUp :: forall (s :: S). Term s PNatRatio -> Term s PNatural
roundUp r = runTermCont (getIntegers r) $ \(n, d) ->
  pif
    (d #== 1)
    (mkNatUnsafe n)
    (mkNatUnsafe $ pdiv # (n + d - (prem # n # d)) # d)

roundDown :: forall (s :: S). Term s PNatRatio -> Term s PNatural
roundDown r = runTermCont (getIntegers r) $ \(n, d) ->
  mkNatUnsafe $ pdiv # (n - (prem # n # d)) # d

-- Auxiliary functions
gt0 :: Integer -> Maybe Natural
gt0 n
  | n < 0 = Nothing
  | otherwise = Just . Natural $ fromInteger n

mkNatRatioUnsafe ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PNatRatio
mkNatRatioUnsafe x y = mkNatRatioUnsafe' # x # y
  where
    mkNatRatioUnsafe' :: Term s (PInteger :--> PInteger :--> PNatRatio)
    mkNatRatioUnsafe' = phoistAcyclic $
      plam $ \x y ->
        pcon . PNatRatio $
          pdcons # pdata (mkNatUnsafe x)
            #$ pdcons # pdata (mkNatUnsafe y) # pdnil

mkNatUnsafe ::
  forall (s :: S).
  Term s PInteger ->
  Term s PNatural
mkNatUnsafe = pcon . PNatural

pgcd :: forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PInteger
pgcd x y = pfix # phoistAcyclic (plam gcd') # x # y # 1
  where
    -- Binary GCD algorithm
    gcd' ::
      forall (s :: S).
      Term s (PInteger :--> PInteger :--> PInteger :--> PInteger) ->
      Term s PInteger ->
      Term s PInteger ->
      Term s PInteger ->
      Term s PInteger
    gcd' self a b d = unTermCont $ do
      aEven <- pletC $ even # a
      bEven <- pletC $ even # b
      pure $
        pif
          (a #== b)
          (a * d)
          ( pif
              aEven
              ( pif
                  bEven
                  (self # half a # half b # d * 2)
                  (self # half a # b # d)
              )
              ( pif
                  bEven
                  (self # a # half b # d)
                  ( pif
                      (a #< b)
                      (self # half (b - a) # a # d)
                      (self # half (a - b) # b # d)
                  )
              )
          )
      where
        even :: Term s (PInteger :--> PBool)
        even = phoistAcyclic $ plam $ \x -> prem # x # 2 #== 0
        half :: Term s PInteger -> Term s PInteger
        half x = pdiv # x # 2

getIntegers ::
  forall (s :: S).
  Term s PNatRatio ->
  TermCont s (Term s PInteger, Term s PInteger)
getIntegers r' = do
  r <- tcont . pletFields @'["numerator", "denominator"] $ r'
  pure (pto $ pfromData r.numerator, pto $ pfromData r.denominator)

natZero :: Term s PNatural
natZero = pconstant $ Natural 0

ratZero :: Term s PNatRatio
ratZero = pconstant . NatRatio $ 0

natOne :: Term s PNatural
natOne = pconstant $ Natural 1

pletC :: forall (s :: S) (a :: PType). Term s a -> TermCont s (Term s a)
pletC = tcont . plet

natPow :: forall (s :: S). Term s (PNatRatio :--> PNatural :--> PNatRatio)
natPow = pfix #$ plam pow
  where
    pow ::
      forall (s :: S).
      Term s (PNatRatio :--> PNatural :--> PNatRatio) ->
      Term s PNatRatio ->
      Term s PNatural ->
      Term s PNatRatio
    pow self base exp = unTermCont $ do
      let exp' = exp #- (pconstant $ Natural 1)
      pure $
        pmatch exp' $ \case
          PNothing ->
            ptraceError
              "natPow: invalid exponent"
          PJust exp'' -> unTermCont $ do
            pure $
              pif
                (exp #== natZero)
                (toNatRatio $ pconstant $ Natural 1)
                (base #* (self # base # exp''))
