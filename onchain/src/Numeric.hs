{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric(
    -- | Typeclasses
    NonNegative((^+), (^*), (^-))
    , PNonNegative((#+), (#*), (#-))
    , Natural(Natural)
    , PNatural
    , NatRatio(NatRatio)
    , PNatRatio
) where

import GHC.Natural qualified as Natural
import GHC.Generics qualified as GHC

import Data.Ratio (
    Ratio
    , numerator
    , denominator
    , (%))

import Plutarch.Lift (
    PUnsafeLiftDecl(PLifted)
    , PConstant(PConstantRepr, PConstanted)
    , pconstantToRepr
    , pconstantFromRepr)
import Plutarch.Builtin (pforgetData)
import Plutarch.Monadic qualified as P

import PlutusTx (
    ToData(toBuiltinData)
    , FromData(fromBuiltinData)
    , UnsafeFromData(unsafeFromBuiltinData)
    , Data(I))
import Plutus.V1.Ledger.Api(
    BuiltinData(BuiltinData))
import Plutarch.Unsafe (punsafeCoerce)

{-
    This module implements some numeric types and operations on them.
    
    In particular, it provides `Natural` and `NatRatio`, which are implemented
    in Plutarch using `PIntegers` and in the Haskell side as `Natural`s from
    the GHC module.
    
    It is meant to be imported qualified, since it redefines some arithmetic
    operations. No `PIntegral` instances are provided, since the methods in
    that class can easily produce negative results.
-}

-- | A natural datatype that wraps GHC's `Natural`. By using `Natural` instead
-- of `Integer` we at least get a warning when using negative literals.
--
-- This datatype *includes* zero.
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
    fromBuiltinData (BuiltinData (I n))
        | n < 0 = Nothing
        | otherwise = Just . Natural $ fromInteger n
    fromBuiltinData _ = Nothing

-- | A natural datatype that wraps a `PInteger`. It derives all of its instances
-- from it, with the exception of `PIntegral`, since its methods can produce
-- non-positive results.
newtype PNatural (s :: S) = PNatural (Term s PInteger)
    deriving stock (GHC.Generic)
    deriving (PEq, POrd, PIsData, PlutusType)
       via (DerivePNewtype PNatural PInteger)

instance PUnsafeLiftDecl PNatural where
    type PLifted PNatural = Natural
    
instance PConstant Natural where
    type PConstantRepr Natural = Integer
    type PConstanted Natural = PNatural
    pconstantToRepr (Natural n) = fromIntegral n
    pconstantFromRepr i
        | i < 0 = Nothing
        | otherwise = Just . Natural $ fromInteger i
        
-- | A rational datatype that wraps a `Ratio Natural`. By using `Natural`
-- instead of `Integer` we at least get a warning when using negative literals.
--
-- This datatype *includes* zero.
newtype NatRatio = NatRatio (Ratio Natural.Natural)
    deriving newtype (Eq, Ord, Show)

instance ToData NatRatio where
    toBuiltinData (NatRatio r) =
        BuiltinData $ plift $ pforgetData . pconstantData . convert $ r
        where convert :: Ratio Natural.Natural -> (Integer, Integer)
              convert x = (fromIntegral $ numerator x, fromIntegral $ denominator x)

instance UnsafeFromData NatRatio where
    unsafeFromBuiltinData x =
        let (n, d) = unsafeFromBuiltinData x :: (Integer, Integer)
        in NatRatio $ fromInteger n % fromInteger d
    
instance FromData NatRatio where
    fromBuiltinData (BuiltinData (I n))
        | n < 0 = Nothing
        | otherwise = Just . NatRatio $ fromInteger n
    fromBuiltinData _ = Nothing
    
-- | A natural datatype that wraps a pair of integers
-- `PBuiltinPair PInteger PInteger`.
newtype PNatRatio (s :: S) =
    PNatRatio 
        (Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
    deriving stock (GHC.Generic)
    deriving (PIsData, PlutusType)
       via (DerivePNewtype PNatRatio (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
       
instance PUnsafeLiftDecl PNatRatio where
    type PLifted PNatRatio = NatRatio
    
instance PConstant NatRatio where
    type PConstantRepr NatRatio = (Integer, Integer)
    type PConstanted NatRatio = PNatRatio
    pconstantToRepr (NatRatio r) =
        (fromIntegral $ numerator r, fromIntegral $ denominator r)
    pconstantFromRepr (n, d)
        | n < 0 || d <= 0 = Nothing
        | otherwise = Just . NatRatio $ (fromInteger n) % (fromInteger d)
        
-- We have to define `PEq` and `POrd` instances manually
instance PEq PNatRatio where
    -- We can assume that fractions are normalized because constant
    -- values are generated from `Ratio`s and all arithmetic operations also
    -- normalize their results
    a #== b = pdata a #== pdata b
    
instance POrd PNatRatio where
    a #<= b = P.do
        (PNatRatio a') <- pmatch a
        (PNatRatio b') <- pmatch b
        let n1 = pfromData $ pfstBuiltin # a'
            d1 = pfromData $ psndBuiltin # a'
            n2 = pfromData $ pfstBuiltin # b'
            d2 = pfromData $ psndBuiltin # b'
        n1 * d2 #<= n2 * d1
    a #< b = P.do
        (PNatRatio a') <- pmatch a
        (PNatRatio b') <- pmatch b
        let n1 = pfromData $ pfstBuiltin # a'
            d1 = pfromData $ psndBuiltin # a'
            n2 = pfromData $ pfstBuiltin # b'
            d2 = pfromData $ psndBuiltin # b'
        n1 * d2 #< n2 * d1

-- | A class for numeric types on which we want safe arithmetic operations that
-- cannot change the signum
class NonNegative a where
    (^+)  :: a -> a -> a
    (^*)  :: a -> a -> a
    (^-)  :: a -> a -> Maybe a

-- | The same as `NonNegative` but for plutarch types
class PNonNegative (a :: PType) where 
    (#+) :: forall (s :: S) . Term s a -> Term s a -> Term s a
    (#*) :: forall (s :: S) . Term s a -> Term s a -> Term s a
    (#-) :: forall (s :: S) . Term s a -> Term s a -> Term s (PMaybe a)

instance NonNegative Natural where
    (^+) :: Natural -> Natural -> Natural
    Natural x ^+ Natural y = Natural $ x + y
    (^*) :: Natural -> Natural -> Natural
    Natural x ^* Natural y = Natural $ x * y
    (^-) :: Natural -> Natural -> Maybe Natural
    Natural x ^- Natural y = if subtraction < 0
        then Nothing
        else Just . Natural $ subtraction
        where subtraction = fromIntegral x - fromIntegral y 
        
-- TODO: Add `PNonNegative` instance for `PInteger`
--       Add `NonNegative` instances for NatRatio