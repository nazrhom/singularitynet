module Utils
  ( jsonReader
  , nat
  , big
  , logInfo_
  , getUtxoWithNFT
  , mkUnbondedPoolParams
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
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
import UnbondedStaking.Types
  ( UnbondedPoolParams(UnbondedPoolParams)
  , InitialUnbondedParams(InitialUnbondedParams)
  )

-- | Helper to decode the local inputs such as unapplied minting policy and
-- typed validator
jsonReader
  :: forall (a :: Type)
   . DecodeJson a
  => String
  -> Json
  -> Either JsonDecodeError a
jsonReader field = caseJsonObject (Left $ TypeMismatch "Expected Object")
  $ flip getField field

-- | Get the UTXO with the NFT defined by its `CurrencySymbol` and `TokenName`.
-- If more than one UTXO contains the NFT, something is seriously wrong.
getUtxoWithNFT
  :: UtxoM
  -> CurrencySymbol
  -> TokenName
  -> Maybe (Tuple TransactionInput TransactionOutput)
getUtxoWithNFT utxoM cs tn =
  let
    utxos = filter hasNFT $ toUnfoldable $ unwrap utxoM
  in
    if length utxos > 1 then Nothing
    else head utxos
  where
  hasNFT
    :: Tuple TransactionInput TransactionOutput
    -> Boolean
  hasNFT (Tuple _ txOutput') =
    let
      txOutput = unwrap txOutput'
      Identity plutusValue = toPlutusType txOutput.amount
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

-- Creates the `UnbondedPoolParams` from the `InitialUnbondedParams` and
-- runtime parameters from the user.
mkUnbondedPoolParams
  :: PaymentPubKeyHash
  -> CurrencySymbol
  -> CurrencySymbol
  -> InitialUnbondedParams
  -> UnbondedPoolParams
mkUnbondedPoolParams admin nftCs assocListCs (InitialUnbondedParams iup) = do
  UnbondedPoolParams
    { start: iup.start
    , userLength: iup.userLength
    , adminLength: iup.adminLength
    , bondingLength: iup.bondingLength
    , interestLength: iup.interestLength
    , increments: iup.increments
    , interest: iup.interest
    , minStake: iup.minStake
    , maxStake: iup.maxStake
    , admin
    , unbondedAssetClass: iup.unbondedAssetClass
    , nftCs
    , assocListCs
    }
