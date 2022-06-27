module ClosePool (closeBondedPoolContract) where

import Contract.Prelude

import BondedStaking.TimeUtils (getClosingTime)
import Contract.Address (getNetworkId, ownPaymentPubKeyHash, scriptHashAddress)
import Contract.Monad (Contract, liftContractM, liftedE', liftedM, logInfo', throwContractError)
import Contract.PlutusData (Datum(..), PlutusData, fromData, getDatumByHash, toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Contract.TxConstraints (TxConstraints, mustBeSignedBy, mustIncludeDatum, mustSpendScriptOutput, mustValidateIn)
import Contract.Utxos (utxosAt)
import Data.Array (elemIndex, (!!))
import Data.Map (toUnfoldable)
import Data.BigInt(BigInt)
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedStakingTokenName)
import Types (BondedPoolParams(BondedPoolParams), BondedStakingAction(CloseAct), BondedStakingDatum)
import Types.Natural (Natural)
import Types.Redeemer (Redeemer(Redeemer))
import Utils (getUtxoWithNFT, logInfo_, splitByLength, submitTransaction, toIntUnsafe, txBatchFinishedCallback)

closeBondedPoolContract
  :: BondedPoolParams
  -> Natural
  -> Array Int
  -> BigInt
  -> Contract () (Array Int)
closeBondedPoolContract
  params@(BondedPoolParams { admin, nftCs })
  batchSize
  closeList
  waitTime = do
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

  -- Get the withdrawing range to use
  logInfo' "closeBondedPoolContract: Getting withdrawing range..."
  { currTime, range: txRange } <- getClosingTime params
  logInfo_ "closeBondedPoolContract: Current time: " $ show currTime
  logInfo_ "closeBondedPoolContract: TX Range" txRange

  -- If closeList is null, update all entries in assocList
  -- Otherwise, just update entries selected by indices in closeList
  spendList <-
    let allConstraints = createUtxoConstraint <$> (toUnfoldable <<< unwrap $ bondedPoolUtxos)
    in if null closeList
      then pure allConstraints
      else liftContractM "depositBondedPoolContract: Failed to create updateList" $
        traverse ((!!) allConstraints) closeList 

  -- We build the transaction
  let
    lookups :: ScriptLookups.ScriptLookups PlutusData
    lookups = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
      , bondedStateDatumLookup
      ]

    constraints :: TxConstraints Unit Unit
    constraints =
      mustBeSignedBy admin
        <> mustIncludeDatum bondedStateDatum
        <> mustValidateIn txRange

  -- Submit transaction with possible batching
  failedDeposits <-
    if batchSize == zero then do
      failedDeposits' <- submitTransaction constraints lookups spendList
      txBatchFinishedCallback waitTime failedDeposits'
      pure failedDeposits'
    else
      let
        updateBatches = splitByLength (toIntUnsafe batchSize) spendList
      in
        mconcat <$> for updateBatches \txBatch -> do
          failedDeposits' <- submitTransaction constraints lookups txBatch
          txBatchFinishedCallback waitTime failedDeposits'
          pure failedDeposits'

  logInfo_
    "closeBondedPoolContract: Finished updating pool entries. /\
    \Entries with failed updates"
    failedDeposits
  -- Return the indices of the failed deposits in the updateList
  liftContractM
    "depositUnbondedPoolContract: Failed to create /\
    \failedDepositsIndicies list" $
    traverse (flip elemIndex spendList) failedDeposits

createUtxoConstraint
  :: Tuple TransactionInput TransactionOutput
  -> Tuple (TxConstraints Unit Unit) (ScriptLookups.ScriptLookups PlutusData)
createUtxoConstraint (input /\ _) = do
  let valRedeemer = Redeemer $ toData CloseAct
  (mustSpendScriptOutput input valRedeemer) /\ mempty
