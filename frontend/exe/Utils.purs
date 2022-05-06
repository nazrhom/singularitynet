module Utils
  ( jsonReader
  , nat
  , big
  , logInfo_
  ) where

import Contract.Prelude

import Contract.Monad (Contract, logInfo, tag)
import Data.Argonaut
  ( Json
  , class DecodeJson
  , JsonDecodeError(TypeMismatch)
  , caseJsonObject
  , getField
  )
import Contract.Numeric.Natural (Natural, fromBigInt')
import Data.BigInt (BigInt, fromInt)

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

logInfo_ :: forall (r :: Row Type). String -> String -> Contract r Unit
logInfo_ k = flip logInfo mempty <<< tag k
