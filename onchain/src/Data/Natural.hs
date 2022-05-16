{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Natural (
  -- | Typeclasses
  NonNegative ((^+), (^*), (^-)),
  PNonNegative ((#+), (#*), (#-)),
  Natural (Natural),
  PNatural,
  NatRatio (NatRatio),
  PNatRatio,
) where

import GHC.Generics qualified as GHC
import GHC.Natural qualified as Natural

import Data.Ratio (
  Ratio,
  denominator,
  numerator,
  (%),
 )

import Plutarch.Builtin (pforgetData)
import Plutarch.Lift (
  PConstant (PConstantRepr, PConstanted),
  PUnsafeLiftDecl (PLifted),
  pconstantFromRepr,
  pconstantToRepr,
 )
import Plutarch.Monadic qualified as P
import Plutarch.TryFrom (PTryFrom)

import Plutus.V1.Ledger.Api (
  BuiltinData (BuiltinData),
 )
import PlutusTx (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )

{-
    This module implements some numeric types and operations on them.

    In particular, it provides `Natural` and `NatRatio`, which are implemented
    in Plutarch using `PIntegers` and in the Haskell side as `Natural`s from
    the GHC module.

    It is meant to be imported qualified, since it redefines some arithmetic
    operations. No `PIntegral` instances are provided, since the methods in
    that class can easily produce negative results.
-}

{- | A natural datatype that wraps GHC's `Natural`. By using `Natural` instead
 of `Integer` we at least get a warning when using negative literals.

 This datatype *includes* zero.
-}
newtype Natural = Natural Natural.Natural
  deriving newtype (Eq, Ord, Show)

-- We need to define the `FromData` and `ToData` instances for `Natural`
-- manually because it uses unlifted types and `unstableMakeIsData` does not
-- work.
instance UnsafeFromData Natural where
  unsafeFromBuiltinData x =
    let n = unsafeFromBuiltinData x :: Integer
     in Natural $ fromInteger n

instance ToData Natural where
  toBuiltinData (Natural n) = toBuiltinData (fromIntegral n :: Integer)

instance FromData Natural where
  fromBuiltinData x = maybe Nothing gt0 $ fromBuiltinData x

{- | A natural datatype that wraps a `PInteger`. It derives all of its instances
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

{- | A rational datatype that wraps a `Ratio Natural`. By using `Natural`
 instead of `Integer` we at least get a warning when using negative literals.

 This datatype *includes* zero.
-}
newtype NatRatio = NatRatio (Ratio Natural.Natural)
  deriving newtype (Eq, Ord, Show)

instance ToData NatRatio where
  toBuiltinData (NatRatio r) =
    BuiltinData $ plift $ pforgetData . pconstantData . toTuple $ r

instance UnsafeFromData NatRatio where
  unsafeFromBuiltinData x =
    let (n, d) = unsafeFromBuiltinData x :: (Integer, Integer)
     in NatRatio $ fromInteger n % fromInteger d

instance FromData NatRatio where
  fromBuiltinData x = maybe Nothing toRatio $ fromBuiltinData x
    where

{- | A natural datatype that wraps a pair of integers
 `PBuiltinPair PInteger PInteger`.
-}
newtype PNatRatio (s :: S)
  = PNatRatio
      (Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
  deriving stock (GHC.Generic)
  deriving
    (PIsData, PlutusType)
    via ( DerivePNewtype
            PNatRatio
            (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
        )

deriving via
  DerivePNewtype
    (PAsData PNatRatio)
    (PAsData (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
  instance
    PTryFrom PData (PAsData PNatRatio)

instance PUnsafeLiftDecl PNatRatio where
  type PLifted PNatRatio = NatRatio

instance PConstant NatRatio where
  type PConstantRepr NatRatio = (Integer, Integer)
  type PConstanted NatRatio = PNatRatio
  pconstantToRepr (NatRatio r) = toTuple r
  pconstantFromRepr = toRatio

-- We have to define `PEq` and `POrd` instances manually
instance PEq PNatRatio where
  -- We can assume that fractions are normalized because constant
  -- values are generated from `Ratio`s and all arithmetic operations also
  -- normalize their results
  a #== b = pdata a #== pdata b

instance POrd PNatRatio where
  a #<= b = P.do
    a' <- plet $ pto a
    b' <- plet $ pto b
    let n1 = pfstData # a'
        d1 = psndData # a'
        n2 = pfstData # b'
        d2 = psndData # b'
    n1 * d2 #<= n2 * d1
  a #< b = P.do
    a' <- plet $ pto a
    b' <- plet $ pto b
    let n1 = pfstData # a'
        d1 = psndData # a'
        n2 = pfstData # b'
        d2 = psndData # b'
    n1 * d2 #< n2 * d1

pfstData ::
  forall (s :: S) (a :: PType) (b :: PType).
  PIsData a =>
  Term s (PBuiltinPair (PAsData a) b :--> a)
pfstData = phoistAcyclic $ plam $ \x -> pfromData $ pfstBuiltin # x

psndData ::
  forall (s :: S) (a :: PType) (b :: PType).
  PIsData b =>
  Term s (PBuiltinPair a (PAsData b) :--> b)
psndData = phoistAcyclic $ plam $ \x -> pfromData $ psndBuiltin # x

{- | A class for numeric types on which we want safe arithmetic operations that
 cannot change the signum
-}
class NonNegative (a :: Type) where
  (^+) :: a -> a -> a
  (^*) :: a -> a -> a
  (^-) :: a -> a -> Maybe a

-- | The same as `NonNegative` but for plutarch types
class PNonNegative (a :: PType) where
  (#+) :: forall (s :: S). Term s a -> Term s a -> Term s a
  (#*) :: forall (s :: S). Term s a -> Term s a -> Term s a
  (#-) :: forall (s :: S). Term s a -> Term s a -> Term s (PMaybe a)

instance NonNegative Natural where
  Natural x ^+ Natural y = Natural $ x + y
  Natural x ^* Natural y = Natural $ x * y
  Natural x ^- Natural y =
    if subtraction < 0
      then Nothing
      else Just . Natural $ subtraction
    where
      subtraction = fromIntegral x - fromIntegral y

instance NonNegative NatRatio where
  NatRatio r ^+ NatRatio q = NatRatio $ r + q
  NatRatio r ^* NatRatio q = NatRatio $ r * q
  NatRatio r ^- NatRatio q =
    if subtraction < 0
      then Nothing
      else Just . NatRatio . fromRational $ subtraction
    where
      subtraction = toRational r - toRational q

instance PNonNegative PNatural where
  x #+ y = pcon . PNatural $ pto x + pto y
  x #* y = pcon . PNatural $ pto x * pto y
  x #- y = P.do
    let x' = pto x
        y' = pto y
    diff <- plet $ x' - y'
    pif
      (diff #< 0)
      (pcon PNothing)
      (pcon . PJust $ pcon . PNatural $ diff)

-- TODO: Add `PNonNegative` instances for NatRatio

-- Auxiliary functions

gt0 :: Integer -> Maybe Natural
gt0 n
  | n < 0 = Nothing
  | otherwise = Just . Natural $ fromInteger n

toTuple :: Ratio Natural.Natural -> (Integer, Integer)
toTuple x = (fromIntegral $ numerator x, fromIntegral $ denominator x)

toRatio :: (Integer, Integer) -> Maybe NatRatio
toRatio (n, d)
  | n < 0 || d <= 0 = Nothing
  | otherwise =
      Just . NatRatio $
        fromInteger n % fromInteger d
