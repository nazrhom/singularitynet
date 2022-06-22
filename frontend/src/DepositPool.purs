module DepositPool (depositBondedPoolContract) where

import Contract.Prelude

import AdminUtils (mkEntryUpdateList)
import Contract.Address (AddressWithNetworkTag(AddressWithNetworkTag), getNetworkId, getWalletAddress, ownPaymentPubKeyHash, scriptHashAddress)
import Contract.Monad (Contract, liftContractM, liftedE', liftedM, logInfo', throwContractError)
import Contract.PlutusData (PlutusData, fromData, getDatumByHash)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.TxConstraints (TxConstraints, mustBeSignedBy)
import Contract.Utxos (utxosAt)
import Control.Applicative (unless)
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedStakingTokenName)
import Types (BondedStakingDatum(StateDatum), BondedPoolParams(BondedPoolParams))
import Types.Natural (Natural)
import Utils (getUtxoWithNFT, logInfo_, mkOnchainAssocList, splitByLength, submitTransaction, toIntUnsafe)

-- Deposits a certain amount in the pool
depositBondedPoolContract ::
  BondedPoolParams ->
  Natural ->
  Array (Tuple (TxConstraints Unit Unit) (ScriptLookups.ScriptLookups PlutusData)) ->
  ( Array (Tuple (TxConstraints Unit Unit) (ScriptLookups.ScriptLookups PlutusData)) -> Contract () Unit) ->
  Contract () ( Array ( Tuple (TxConstraints Unit Unit) (ScriptLookups.ScriptLookups PlutusData)))
depositBondedPoolContract
  params@
    ( BondedPoolParams
        { admin
        , nftCs
        , assocListCs
        }
    )
  batchSize
  depositList
  batchFinishCallback = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "depositBondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "depositBondedPoolContract: Admin is not current user"
  logInfo_ "depositBondedPoolContract: Admin PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  AddressWithNetworkTag { address: adminAddr } <-
    liftedM "depositBondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "depositBondedPoolContract: Cannot get user Utxos"
      $ utxosAt adminAddr
  -- Get the bonded pool validator and hash
  validator <- liftedE' "depositBondedPoolContract: Cannot create validator"
    $ mkBondedPoolValidator params
  valHash <- liftContractM "depositBondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "depositBondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "depositBondedPoolContract: Pool address"
    $ fromPlutusType (networkId /\ poolAddr)
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM
      "depositBondedPoolContract: Cannot get pool's utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "depositBondedPoolContract: Pool UTXOs" bondedPoolUtxos
  tokenName <- liftContractM
    "depositBondedPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "depositBondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT bondedPoolUtxos nftCs tokenName
  logInfo_ "depositBondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "depositBondedPoolContract: Could not get Pool UTXO's Datum Hash"
      (unwrap poolTxOutput).dataHash
  logInfo_ "depositBondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "depositBondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  bondedStakingDatum :: BondedStakingDatum <-
    liftContractM
      "depositBondedPoolContract: Cannot extract NFT State datum"
      $ fromData (unwrap poolDatum)

  -- Update the association list
  case bondedStakingDatum of
    -- Non-empty user list
    StateDatum { maybeEntryName: Just _ } -> do
      logInfo'
        "depositBondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Just ... }"
      let
        assocList = mkOnchainAssocList assocListCs bondedPoolUtxos
        -- Concatenate constraints/lookups
        constraints :: TxConstraints Unit Unit
        constraints = mustBeSignedBy admin

        lookups :: ScriptLookups.ScriptLookups PlutusData
        lookups =
          ScriptLookups.validator validator
            <> ScriptLookups.unspentOutputs (unwrap adminUtxos)
            <> ScriptLookups.unspentOutputs (unwrap bondedPoolUtxos)

      -- Use depositList as updateList if not null, otherwise update stakes of all users
      updateList <- if not (null depositList)
        then pure depositList
        else traverse (mkEntryUpdateList params valHash) assocList
      -- Submit transaction with possible batching
      failedDeposits <-
        if batchSize == zero then do
          failedDeposits' <- submitTransaction constraints lookups updateList
          batchFinishCallback failedDeposits'
          pure failedDeposits'
        else
          let updateBatches = splitByLength (toIntUnsafe batchSize) updateList
          in mconcat <$> for updateBatches \txBatch -> do
            failedDeposits' <- submitTransaction constraints lookups txBatch 
            batchFinishCallback failedDeposits'
            pure failedDeposits'
      logInfo_
        "depositBondedPoolContract: Finished updating pool entries. /\
        \Entries with failed updates"
        failedDeposits
      pure failedDeposits
  -- Other error cases:
    StateDatum { maybeEntryName: Nothing } ->
      throwContractError
        "depositBondedPoolContract: There are no users in the pool to deposit \
        \rewards for"
    _ ->
      throwContractError "depositBondedPoolContract: Datum incorrect type"
