module Utils
  ( big
  , findAssocElem
  , getUtxoWithNFT
  , hashPkh
  , jsonReader
  , logInfo_
  , mkBondedPoolParams
  , mkOnchainAssocList
  , mkUnbondedPoolParams
  , nat
  ) where

import Contract.Prelude hiding (length)

import Contract.Address (PaymentPubKeyHash)
import Contract.Hashing (blake2b256Hash)
import Contract.Monad (Contract, logInfo, tag)
import Contract.Numeric.Natural (Natural, fromBigInt')
import Contract.Prim.ByteArray (ByteArray, hexToByteArray)
import Contract.Scripts (PlutusScript)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  )
import Contract.Utxos (UtxoM(UtxoM))
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , flattenNonAdaAssets
  , getTokenName
  , valueOf
  )
import Control.Alternative (guard)
import Data.Argonaut.Core (Json, caseJsonObject)
import Data.Argonaut.Decode.Combinators (getField) as Json
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch))
import Data.Array (filter, head, last, length, partition, mapMaybe, sortBy)
import Data.BigInt (BigInt, fromInt)
import Data.Map (toUnfoldable)
import Serialization.Hash (ed25519KeyHashToBytes)
import Types
  ( BondedPoolParams(BondedPoolParams)
  , InitialBondedParams(InitialBondedParams)
  , MintingAction(MintEnd, MintInBetween)
  )
import UnbondedStaking.Types
  ( UnbondedPoolParams(UnbondedPoolParams)
  , InitialUnbondedParams(InitialUnbondedParams)
  )

-- | Helper to decode the local inputs such as unapplied minting policy and
-- typed validator
jsonReader
  :: String
  -> Json
  -> Either JsonDecodeError PlutusScript
jsonReader field = do
  caseJsonObject (Left $ TypeMismatch "Expected Object") $ \o -> do
    hex <- Json.getField o field
    case hexToByteArray hex of
      Nothing -> Left $ TypeMismatch "Could not convert to bytes"
      Just bytes -> pure $ wrap bytes

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

hashPkh :: PaymentPubKeyHash -> ByteArray
hashPkh =
  blake2b256Hash <<< unwrap <<< ed25519KeyHashToBytes <<< unwrap <<< unwrap

-- | Makes an on chain assoc list returning the key, input and output. We could
-- | be more stringent on checks to ensure the list is genuinely connected
-- | although on chain code should enforce this.
mkOnchainAssocList
  :: BondedPoolParams
  -> UtxoM
  -> Array (ByteArray /\ TransactionInput /\ TransactionOutput)
mkOnchainAssocList (BondedPoolParams { assocListCs }) (UtxoM utxos) =
  sortBy compareBytes $ mapMaybe getAssocListUtxos $ toUnfoldable utxos
  where
  getAssocListUtxos
    :: TransactionInput /\ TransactionOutput
    -> Maybe (ByteArray /\ TransactionInput /\ TransactionOutput)
  getAssocListUtxos utxo@(_ /\ (TransactionOutput txOutput)) = do
    let val = flattenNonAdaAssets txOutput.amount
    cs /\ tn /\ amt <- head val
    guard (length val == one && cs == assocListCs && amt == one)
    pure $ (unwrap $ getTokenName tn) /\ utxo

compareBytes
  :: forall (t :: Type). ByteArray /\ t -> ByteArray /\ t -> Ordering
compareBytes (bytes /\ _) (bytes' /\ _) = compare bytes bytes'

-- | Find the assoc list element to update or insert. This can be optimised
-- | if we compare pairs and exit early of course. But we'll do this for
-- | simplicity. THIS MUST BE USED ON A SORTED LIST, i.e. with
-- | `mkOnchainAssocList`. We should probably create a type for the output.
findAssocElem
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutput)
  -> ByteArray
  -> Maybe
       ( Maybe MintingAction
           /\
             { firstInput :: TransactionInput
             , secondInput :: Maybe TransactionInput
             }
           /\
             { firstOutput :: TransactionOutput
             , secondOutput :: Maybe TransactionOutput
             }
           /\
             { firstKey :: ByteArray
             , secondKey :: Maybe ByteArray
             }
       )
findAssocElem assocList hashedKey = do
  -- The list should findAssocElem assocList hashedKey = do be sorted so no
  -- need to resort
  let { no, yes } = partition (fst >>> (>=) hashedKey) assocList
  bytesL /\ txInputL /\ txOutputL <- last yes
  -- If we're at the last element, it must be an end stake or updating last
  -- element
  if length no == zero then do
    -- Workout whether it's an initial deposit
    let
      mintingAction =
        if bytesL == hashedKey then Nothing
        else Just $ MintEnd txInputL
    pure
      $ mintingAction
      /\ { firstInput: txInputL, secondInput: Nothing }
      /\ { firstOutput: txOutputL, secondOutput: Nothing }
      /\ { firstKey: bytesL, secondKey: Nothing }
  -- Otherwise, it is an inbetween stake or updating the first element
  else do
    bytesH /\ txInputH /\ txOutputH <- head no
    let
      mintingAction =
        if bytesL == hashedKey then Nothing
        else Just $ MintInBetween txInputL txInputH
    pure
      $ mintingAction
      /\ { firstInput: txInputL, secondInput: Just txInputH }
      /\ { firstOutput: txOutputL, secondOutput: Just txOutputH }
      /\ { firstKey: bytesL, secondKey: Just bytesH }