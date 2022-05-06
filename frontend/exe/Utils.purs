module Utils(
    jsonReader
    , nat
    , big
) where

import Contract.Prelude

import Data.Argonaut (Json, class DecodeJson, JsonDecodeError(TypeMismatch), caseJsonObject, getField)
import Contract.Numeric.Natural(Natural, fromBigInt')
import Data.BigInt(BigInt, fromInt)

-- | Helper to decode the local inputs such as unapplied minting policy and
-- | typed validator
jsonReader
  :: forall (a :: Type)
   . DecodeJson a
  => String
  -> Json
  -> Either JsonDecodeError a
jsonReader field = caseJsonObject (Left $ TypeMismatch "Expected Object")
  $ flip getField field

-- | Convert from `Int` to `Natural`
nat :: Int -> Natural
nat = fromBigInt' <<< fromInt
-- | Convert from `Int` to `BigInt`
big :: Int -> BigInt
big = fromInt