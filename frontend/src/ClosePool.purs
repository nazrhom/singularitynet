module ClosePool (closeBondedPoolContract) where

import Contract.Prelude

import AdminUtils (mkEntryUpdateList)
import Contract.Address (getNetworkId, ownPaymentPubKeyHash, scriptHashAddress)
import Contract.Monad (Contract, liftContractM, liftedE', liftedM, throwContractError)
import Contract.PlutusData (Datum(Datum), PlutusData, fromData, getDatumByHash, toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.TxConstraints (TxConstraints, mustBeSignedBy, mustIncludeDatum)
import Contract.Utxos (utxosAt)
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedStakingTokenName)
import Types (BondedPoolParams(BondedPoolParams), BondedStakingDatum)
import Types.Natural (Natural)
import Utils (getUtxoWithNFT, logInfo_, mkOnchainAssocList, splitByLength, submitTransaction, toIntUnsafe)

closeBondedPoolContract ::
  BondedPoolParams ->
  Natural ->
  Array (Tuple (TxConstraints Unit Unit) (ScriptLookups.ScriptLookups PlutusData)) ->
  ( Array (Tuple (TxConstraints Unit Unit) (ScriptLookups.ScriptLookups PlutusData)) -> Contract () Unit) ->
  Contract () ( Array ( Tuple (TxConstraints Unit Unit) (ScriptLookups.ScriptLookups PlutusData)))
closeBondedPoolContract
  params@(BondedPoolParams { admin, nftCs })
  batchSize
  closeList
  batchFinishCallback = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "closeBondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "closeBondedPoolContract: Admin \
    \is not current user"
  logInfo_ "closeBondedPoolContract: Admin PaymentPubKeyHash" admin
  -- Get the bonded pool validator and hash
  validator <- liftedE' "closeBondedPoolContract: Cannot create validator"
    $ mkBondedPoolValidator params
  valHash <- liftContractM "closeBondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "closeBondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "closeBondedPoolContract: Pool address"
    $ fromPlutusType (networkId /\ poolAddr)
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM "closeBondedPoolContract: Cannot get pool's utxos at pool address" $
      utxosAt poolAddr
  logInfo_ "closeBondedPoolContract: Pool's UTXOs" bondedPoolUtxos
  tokenName <- liftContractM
    "closeBondedPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "closeBondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT bondedPoolUtxos nftCs tokenName
  logInfo_ "closeBondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "closeBondedPoolContract: Could not get Pool UTXO's Datum Hash"
      (unwrap poolTxOutput).dataHash
  logInfo_ "closeBondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "closeBondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  bondedStakingDatum :: BondedStakingDatum <-
    liftContractM
      "closeBondedPoolContract: Cannot extract NFT State datum"
      $ fromData (unwrap poolDatum)
  let bondedStateDatum = Datum $ toData bondedStakingDatum
  bondedStateDatumLookup <-
    liftContractM
      "closeBondedPoolContract: Could not create state datum lookup"
      $ ScriptLookups.datum bondedStateDatum
  -- We build the transaction
  let
    lookups :: ScriptLookups.ScriptLookups PlutusData
    lookups = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
      , bondedStateDatumLookup
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      -- Spend all UTXOs to return to Admin:
      --foldMap
      --  (flip mustSpendScriptOutput redeemer <<< fst)
      --  (toUnfoldable $ unwrap bondedPoolUtxos :: Array _)
      mustBeSignedBy admin
        <> mustIncludeDatum bondedStateDatum

    assocList = mkOnchainAssocList nftCs bondedPoolUtxos

  -- Use depositList as updateList if not null, otherwise update stakes of all users
  updateList <- if not (null closeList)
    then pure closeList
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