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
) where

import GHC.Generics qualified as GHC

import SingularityNet.Natural (
  NatRatio (NatRatio),
  Natural (Natural),
  toRatio,
  toTuple,
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
  a #<= b = unTermCont $ do
    a' <- pletC $ pto a
    b' <- pletC $ pto b
    let n1 = pfstData a'
        d1 = psndData a'
        n2 = pfstData b'
        d2 = psndData b'
    pure $ n1 * d2 #<= n2 * d1
  a #< b = unTermCont $ do
    a' <- pletC $ pto a
    b' <- pletC $ pto b
    let n1 = pfstData a'
        d1 = psndData a'
        n2 = pfstData b'
        d2 = psndData b'
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

-- TODO: Add `PNonNegative` instances for NatRatio

-- Auxiliary functions
gt0 :: Integer -> Maybe Natural
gt0 n
  | n < 0 = Nothing
  | otherwise = Just . Natural $ fromInteger n

pfstData ::
  forall (s :: S) (a :: PType) (b :: PType).
  PIsData a =>
  Term s (PBuiltinPair (PAsData a) b) ->
  Term s a
pfstData x = pfromData $ pfstBuiltin # x

psndData ::
  forall (s :: S) (a :: PType) (b :: PType).
  PIsData b =>
  Term s (PBuiltinPair a (PAsData b)) ->
  Term s b
psndData x = pfromData $ psndBuiltin # x

pletC :: forall (s :: S) (a :: PType). Term s a -> TermCont s (Term s a)
pletC = tcont . plet
