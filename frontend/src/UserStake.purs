module UserStake (userStakeBondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( AddressWithNetworkTag(AddressWithNetworkTag)
  , PaymentPubKeyHash(PaymentPubKeyHash)
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Hashing (blake2b256Hash)
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , throwContractError
  )
import Contract.PlutusData
  ( PlutusData
  , Datum(Datum)
  , datumHash
  , fromData
  , getDatumByHash
  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Contract.Utxos (UtxoM(UtxoM), utxosAt)
import Contract.Value (TokenName, flattenValue, getTokenName, singleton)
import Control.Alternative (guard)
import Control.Applicative (unless)
import Data.Array (filter, head, last, length, mapMaybe, sortBy)
import Data.Map (toUnfoldable)
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedStakingTokenName)
import Serialization.Hash (ed25519KeyHashToBytes)
import Types
  ( BondedStakingAction(AdminAct)
  , BondedStakingDatum (AssetDatum, EntryDatum, StateDatum)
  , BondedPoolParams(BondedPoolParams)
  , InsertionType (Head, Tail)
  , UserStakeType (Initial, Further)
  )
import Types.Redeemer (Redeemer(Redeemer))
import Utils (big, getUtxoWithNFT, hashPkh, logInfo_, nat)

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
    let val = flattenValue txOutput.amount
    cs /\ tn /\ amt <- head val
    guard (length val == one && cs == assocListCs && amt == one)
    pure $ (unwrap $ getTokenName tn) /\ utxo

-- | Find the assoc list element to update or insert. This can be optimised
-- | if we compare pairs and exit early of course. But we'll do this for
-- | simplicity
findAssocElem
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutput)
  -> ByteArray
  -> Maybe (UserStakeType /\ TransactionInput /\ TransactionOutput)
findAssocElem assocList hashedKey = do
  -- The list should already be sorted so no need to resort
  bytes /\ txInput /\ txOutput <- last $ filter (fst >>> (>=) hashedKey) assocList
  if hashedKey == bytes then pure (Further /\ txInput /\ txOutput)
  else pure $ Initial /\ txInput /\ txOutput  -- hashedKey > bytes

compareBytes :: ByteArray /\ _ -> ByteArray /\ _ -> Ordering
compareBytes (bytes /\ _) (bytes' /\ _) = compare bytes bytes'

-- Deposits a certain amount in the pool
userStakeBondedPoolContract :: BondedPoolParams -> Natural -> Contract () Unit
userStakeBondedPoolContract params@(BondedPoolParams { admin, nftCs }) amt = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "userStakeBondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  logInfo_ "userStakeBondedPoolContract: User's PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  AddressWithNetworkTag { address: userAddr } <-
    liftedM "userStakeBondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  userUtxos <-
    liftedM "userStakeBondedPoolContract: Cannot get user Utxos"
      $ utxosAt userAddr
  -- Get the bonded pool validator and hash
  validator <- liftedE' "userStakeBondedPoolContract: Cannot create validator"
    $ mkBondedPoolValidator params
  valHash <- liftContractM "userStakeBondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "userStakeBondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "userStakeBondedPoolContract: Pool address"
    $ fromPlutusType (networkId /\ poolAddr)
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM
      "userStakeBondedPoolContract: Cannot get pool's utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "userStakeBondedPoolContract: Pool UTXOs" bondedPoolUtxos
  tokenName <- liftContractM
    "userStakeBondedPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "userStakeBondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT bondedPoolUtxos nftCs tokenName
  logInfo_ "userStakeBondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "userStakeBondedPoolContract: Could not get Pool UTXO's Datum Hash"
      (unwrap poolTxOutput).dataHash
  logInfo_ "userStakeBondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "userStakeBondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  bondedStakingDatum :: BondedStakingDatum <- liftContractM
    "userStakeBondedPoolContract: Cannot extract NFT State datum"
    $ fromData (unwrap poolDatum)
  let hashedUserPkh = hashPkh userPkh
  insertionType /\ userStakeType /\ txIn /\ txOut <- case bondedStakingDatum of
    StateDatum { maybeEntryName: Nothing } ->
      -- If the state UTXO has nothing, it is a head deposit, spending the state
      -- UTXO
      pure $ Head /\ Initial /\ poolTxInput /\ poolTxOutput
    StateDatum { maybeEntryName: Just key } -> do
      let assocList = mkOnchainAssocList params bondedPoolUtxos
      -- If hashedUserPkh < key, we have a head deposit, spending the state utxo
      -- If hashedUserPkh == key, this is a further deposit spending the first
      -- assoc. list element.
      -- If hashedUserPkh > key, we filter elements and find the last suitable
      -- one.
      case compare hashedUserPkh key of
        LT -> pure $ Head /\ Initial /\ poolTxInput /\ poolTxOutput
        EQ -> do
          assocElem <- liftContractM "userStakeBondedPoolContract: Cannot extract\
            \head from Assoc. List - this should be impossible" $ head assocList
          pure $ Tail /\ Further /\ snd assocElem
        GT -> liftContractM
          "userStakeBondedPoolContract: Cannot get tail element"
          $ (Tail /\ _) <$> findAssocElem assocList hashedUserPkh
    _ -> throwContractError "findAssocLuserStakeBondedPoolContractistUtxo: \
          \Datum incorrect type"

  dHash <-  liftContractM
    "userStakeBondedPoolContract: Could not get spending UTXO's Datum Hash"
    (unwrap txOut).dataHash
  logInfo_ "userStakeBondedPoolContract: Spending UTXO DatumHash" dHash
  listDatum <- liftedM "userStakeBondedPoolContract: Cannot get spending datum"
    $ getDatumByHash poolDatumHash
  bondedListDatum :: BondedStakingDatum <- liftContractM
    "userStakeBondedPoolContract: Cannot extract NFT State datum"
    $ fromData (unwrap listDatum)

  case bondedListDatum of
    StateDatum { maybeEntryName: Nothing } -> do
      


  -- -- Create the datums and their ScriptLookups
  -- let
  --   -- We can hardcode the state for now. We should actually fetch the datum
  --   -- from Ogmios, update it properly and then submit it
  --   bondedStateDatum = Datum $ toData $ StateDatum
  --     { maybeEntryName: Nothing
  --     , sizeLeft: nat 100_000_000
  --     }
  --   -- This is the datum of the UTXO that will hold the rewards
  --   assetDatum = Datum $ toData AssetDatum

  -- bondedStateDatumLookup <-
  --   liftContractM
  --     "userStakeBondedPoolContract: Could not create state datum lookup"
  --     $ ScriptLookups.datum bondedStateDatum
  let
  --   assetParams = unwrap (unwrap params).bondedAssetClass
  --   assetCs = assetParams.currencySymbol
  --   assetTn = assetParams.tokenName
  --   stateTokenValue = singleton nftCs tokenName one
  --   depositValue = singleton assetCs assetTn (big 2)

    redeemerData = toData $ BondedStakingAction { stakeAmount: amt, stakeHolder: userPkh }
    redeemer = Redeemer redeemerData

  --   lookup :: ScriptLookups.ScriptLookups PlutusData
  --   lookup = mconcat
  --     [ ScriptLookups.validator validator
  --     , ScriptLookups.unspentOutputs $ unwrap adminUtxos
  --     , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
  --     , bondedStateDatumLookup
  --     ]

  --   -- Seems suspect, not sure if typed constraints are working as expected
  --   constraints :: TxConstraints Unit Unit
  --   constraints =
  --     mconcat
  --       [
  --         -- Update the pool's state
  --         mustPayToScript valHash bondedStateDatum stateTokenValue
  --       -- Deposit rewards in a separate UTXO
  --       , mustPayToScript valHash assetDatum depositValue
  --       , mustBeSignedBy admin
  --       , mustSpendScriptOutput poolTxInput redeemer
  --       ]
  -- dh <- liftContractM "userStakeBondedPoolContract: Cannot Hash AssetDatum"
  --   $ datumHash assetDatum
  -- dh' <- liftContractM "userStakeBondedPoolContract: Cannot Hash BondedStateDatum"
  --   $ datumHash bondedStateDatum
  -- logInfo_ "userStakeBondedPoolContract: DatumHash of AssetDatum" dh
  -- logInfo_ "userStakeBondedPoolContract: DatumHash of BondedStateDatum" dh'
  -- unattachedBalancedTx <-
  --   liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  -- logInfo_
  --   "userStakeBondedPoolContract: unAttachedUnbalancedTx"
  --   unattachedBalancedTx
  -- BalancedSignedTransaction { signedTxCbor } <-
  --   liftedM
  --     "userStakeBondedPoolContract: Cannot balance, reindex redeemers, attach \
  --     \datums redeemers and sign"
  --     $ balanceAndSignTx unattachedBalancedTx
  -- -- Submit transaction using Cbor-hex encoded `ByteArray`
  -- transactionHash <- submit signedTxCbor
  -- logInfo_
  --   "userStakeBondedPoolContract: Transaction successfully submitted with hash"
  --   $ byteArrayToHex
  --   $ unwrap transactionHash

  pure unit