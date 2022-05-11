module Utils
  ( jsonReader
  , nat
  , big
  , logInfo_
  , getUtxoWithNFT
  ) where

import Contract.Prelude

import Contract.Monad (Contract, logInfo, tag)
import Contract.Numeric.Natural (Natural, fromBigInt')
import Contract.Value (TokenName)
import Data.Argonaut
  ( Json
  , class DecodeJson
  , JsonDecodeError(TypeMismatch)
  , caseJsonObject
  , getField
  )
import Data.Array (filter, head)
import Data.BigInt (BigInt, fromInt)
import Data.Identity (Identity(Identity))
import Data.Map (toUnfoldable)
import Plutus.ToPlutusType (toPlutusType)
import Plutus.Types.CurrencySymbol (CurrencySymbol)
import Plutus.Types.Value (valueOf)
import Types.Transaction (TransactionInput, TransactionOutput, UtxoM)

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

-- | Get the UTXO with the NFT defined by its `CurrencySymbol` and `TokenName`.
-- If more than one UTXO contains the NFT, something is seriously wrong and
-- fails with en error message.
getUtxoWithNFT
  :: UtxoM
  -> CurrencySymbol
  -> TokenName
  -> Maybe (Tuple TransactionInput TransactionOutput)
getUtxoWithNFT utxoM cs tn = head $ filter (hasNFT cs tn)
  $ toUnfoldable
  $ unwrap utxoM
  where
  hasNFT
    :: CurrencySymbol
    -> TokenName
    -> Tuple TransactionInput TransactionOutput
    -> Boolean
  hasNFT cs tn (Tuple _txInput txOutput') =
    let
      txOutput = unwrap txOutput'
      (Identity plutusValue) = toPlutusType txOutput.amount
    in
      valueOf plutusValue cs tn == one

-- | Convert from `Int` to `Natural`
nat :: Int -> Natural
nat = fromBigInt' <<< fromInt

-- | Convert from `Int` to `BigInt`
big :: Int -> BigInt
big = fromInt

logInfo_
  :: forall (r :: Row Type) (a :: Type)
   . Show a
  => String
  -> a
  -> Contract r Unit
logInfo_ k = flip logInfo mempty <<< tag k <<< show
