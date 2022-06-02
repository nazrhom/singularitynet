module Utils
  ( big
  , getUtxoWithNFT
  , jsonReader
  , logInfo_
  , mkBondedPoolParams
  , mkUnbondedPoolParams
  , nat
  ) where

import Contract.Prelude

import Aeson
  ( Aeson
  , class DecodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , getField
  )
import Contract.Address (PaymentPubKeyHash)
import Contract.Monad (Contract, logInfo, tag)
import Contract.Numeric.Natural (Natural, fromBigInt')
import Contract.Transaction (TransactionInput, TransactionOutput)
import Contract.Utxos (UtxoM)
import Contract.Value (CurrencySymbol, TokenName, valueOf)
import Data.Array (filter, head)
import Data.BigInt (BigInt, fromInt)
import Data.Map (toUnfoldable)
import Types
  ( BondedPoolParams(BondedPoolParams)
  , InitialBondedParams(InitialBondedParams)
  )
import UnbondedStaking.Types
  ( UnbondedPoolParams(UnbondedPoolParams)
  , InitialUnbondedParams(InitialUnbondedParams)
  )

-- | Helper to decode the local inputs such as unapplied minting policy and
-- typed validator
jsonReader
  :: forall (a :: Type)
   . DecodeAeson a
  => String
  -> Aeson
  -> Either JsonDecodeError a
jsonReader field = caseAesonObject (Left $ TypeMismatch "Expected Object")
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
    in
      valueOf txOutput.amount cs tn == one

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

-- Creates the `BondedPoolParams` from the `InitialBondedParams` and runtime
-- parameters from the user.
mkBondedPoolParams
  :: PaymentPubKeyHash
  -> CurrencySymbol
  -> CurrencySymbol
  -> InitialBondedParams
  -> BondedPoolParams
mkBondedPoolParams admin nftCs assocListCs (InitialBondedParams ibp) = do
  BondedPoolParams
    { iterations: ibp.iterations
    , start: ibp.start
    , end: ibp.end
    , userLength: ibp.userLength
    , bondingLength: ibp.bondingLength
    , interest: ibp.interest
    , minStake: ibp.minStake
    , maxStake: ibp.maxStake
    , admin
    , bondedAssetClass: ibp.bondedAssetClass
    , nftCs
    , assocListCs
    }

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
