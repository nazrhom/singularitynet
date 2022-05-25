module UnbondedStaking.CloseUnbondedPool (closeUnbondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , ownPaymentPubKeyHash
  , validatorHashEnterpriseAddress
  )
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , throwContractError
  )
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData
  , Redeemer(Redeemer)
  , toData
  )
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Time (always)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustIncludeDatum
  , mustPayToScript
  , mustSpendScriptOutput
  , mustValidateIn
  )
import Contract.Utxos (utxosAt)
import Contract.Value (singleton)
import Data.Map (toUnfoldable)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings (unbondedStakingTokenName)
import UnbondedStaking.Types
  ( UnbondedPoolParams(UnbondedPoolParams)
  , UnbondedStakingAction(CloseAct)
  , UnbondedStakingDatum(StateDatum)
  )
import Utils (getUtxoWithNFT, logInfo_)

closeUnbondedPoolContract :: UnbondedPoolParams -> Contract () Unit
closeUnbondedPoolContract params@(UnbondedPoolParams { admin, nftCs }) = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "closeUnbondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "closeUnbondedPoolContract: Admin is not current user"
  logInfo_ "closeUnbondedPoolContract: Admin PaymentPubKeyHash" admin

  -- Get the bonded pool validator and hash
  validator <- liftedE' "closeUnbondedPoolContract: Cannot create validator" $
    mkUnbondedPoolValidator params
  valHash <- liftedM "closeUnbondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "closeUnbondedPoolContract: validatorHash" valHash
  let poolAddr = validatorHashEnterpriseAddress networkId valHash
  logInfo_ "closeUnbondedPoolContract: Pool address" poolAddr

  -- Get the bonded pool's utxo
  unbondedPoolUtxos <-
    liftedM
      "closeUnbondedPoolContract: Cannot get pool's utxos at pool address" $
      utxosAt poolAddr
  logInfo_ "closeUnbondedPoolContract: Pool's UTXOs" unbondedPoolUtxos

  -- Find the state datum
  tokenName <- liftContractM
    "closeUnbondedPoolContract: Cannot create TokenName"
    unbondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "closeUnbondedPoolContract: Cannot get state utxo" $
      getUtxoWithNFT unbondedPoolUtxos nftCs tokenName
  logInfo_ "closeUnbondedPoolContract: Pool's State UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "closeUnbondedPoolContract: Could not get Pool UTXO's Datum Hash"
      (unwrap poolTxOutput).dataHash
  logInfo_ "closeUnbondedPoolContract: Pool's UTXO DatumHash" poolDatumHash

  let
    stateTokenValue = singleton nftCs tokenName one
    oldUnbondedStateDatum = Datum $ toData StateDatum
      { maybeEntryName: Nothing
      , open: true
      }
    newUnbondedStateDatum = Datum $ toData StateDatum
      { maybeEntryName: Nothing
      , open: false
      }

  unbondedStateDatumLookup <-
    liftContractM
      "closeUnbondedPoolContract: Could not create state datum lookup"
      =<< ScriptLookups.datum oldUnbondedStateDatum

  -- We build the transaction
  let
    redeemer = Redeemer $ toData CloseAct

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap unbondedPoolUtxos
      , unbondedStateDatumLookup
      ]

    constraints :: TxConstraints Unit Unit
    constraints =
      -- TODO: Only allow admin to withdraw state and remaining asset UTXOs
      -- TODO: Update entry UTXOs in assoc list with updated rewards

      -- Spend all UTXOs to return to Admin the state and remaining asset UTXO
      foldMap
        (flip mustSpendScriptOutput redeemer <<< fst)
        (toUnfoldable $ unwrap unbondedPoolUtxos :: Array _)
        <> mustBeSignedBy admin
        <> mustIncludeDatum oldUnbondedStateDatum

        -- Update the pool's state to closed
        <> mustPayToScript valHash newUnbondedStateDatum stateTokenValue

        -- TODO: Validate transaction within current/next adminLength
        <> mustValidateIn always

  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "closeUnbondedPoolContract: Cannot balance, reindex redeemers, attach/\
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx

  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "closeUnbondedPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash
