{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Natural (
  -- | Typeclasses
  NonNegative ((^+), (^*), (^-)),
  Natural (Natural),
  NatRatio (NatRatio),
  toTuple,
  toRatio,
) where

{-
    This module implements some numeric types and operations on them.

    In particular, it provides `Natural` and `NatRatio`, which are implemented
    in the Haskell side as `Natural`s from the GHC module.
-}

import GHC.Natural qualified as Natural

import Data.Ratio (
  Ratio,
  denominator,
  numerator,
  (%),
 )

import Plutus.V1.Ledger.Api (BuiltinData (BuiltinData), toData)
import PlutusTx (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )

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

{- | A rational datatype that wraps a `Ratio Natural`. By using `Natural`
 instead of `Integer` we at least get a warning when using negative literals.

 This datatype *includes* zero.
-}
newtype NatRatio = NatRatio (Ratio Natural.Natural)
  deriving newtype (Eq, Ord, Show)

instance ToData NatRatio where
  toBuiltinData (NatRatio r) =
    BuiltinData $ toData . toTuple $ r

instance UnsafeFromData NatRatio where
  unsafeFromBuiltinData x =
    let (n, d) = unsafeFromBuiltinData x :: (Integer, Integer)
     in NatRatio $ fromInteger n % fromInteger d

instance FromData NatRatio where
  fromBuiltinData x = maybe Nothing toRatio $ fromBuiltinData x
    where

{- | A class for numeric types on which we want safe arithmetic operations that
 cannot change the signum
-}
class NonNegative (a :: Type) where
  (^+) :: a -> a -> a
  (^*) :: a -> a -> a
  (^-) :: a -> a -> Maybe a

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
