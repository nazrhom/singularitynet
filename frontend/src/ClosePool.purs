module ClosePool (closeBondedPoolContract) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (Contract, liftContractM, liftedE, liftedE', liftedM)
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData
  , Redeemer(Redeemer)
  , toData
  )
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustIncludeDatum
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Data.Map (toUnfoldable)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedHardCodedParams)
import Types
  ( BondedStakingAction(..)
  , BondedStakingDatum(StateDatum)
  , PoolInfo(PoolInfo)
  )
import Utils (logInfo_, nat)

closeBondedPoolContract :: PoolInfo -> Contract () Unit
closeBondedPoolContract (PoolInfo poolInfo) = do
  -- Get fields from pool info
  let
    poolAddr = poolInfo.poolAddr
    nftCs = poolInfo.stateNftCs
    assocListCs = poolInfo.assocListCs
  adminPkh <- liftedM "closeBondedPoolContract: Cannot get admin's pkh"
    ownPaymentPubKeyHash
  logInfo_ "closeBondedPoolContract: Admin PaymentPubKeyHash" adminPkh
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM "closeBondedPoolContract: Cannot get pool's utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "closeBondedPoolContract: Pool's UTXOs" bondedPoolUtxos
  -- Create parameters of the pool and validator
  params <-
    liftContractM
      "closeBondedPoolContract: Failed to create parameters"
      $ bondedHardCodedParams adminPkh nftCs assocListCs
  validator <- liftedE' "closeBondedPoolContract: Cannot create validator" $
    mkBondedPoolValidator params
  let
    bondedStateDatum = Datum $ toData StateDatum
      { maybeEntryName: Nothing
      , sizeLeft: nat 100_000_000
      }
  bondedStateDatumLookup <-
    liftContractM
      "closeBondedPoolContract: Could not create state datum lookup"
      =<< ScriptLookups.datum bondedStateDatum
  -- We build the transaction
  let
    redeemer = Redeemer $ toData CloseAct

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
      , bondedStateDatumLookup
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      -- Spend all UTXOs to return to Admin:
      foldMap
        (flip mustSpendScriptOutput redeemer <<< fst)
        (toUnfoldable $ unwrap bondedPoolUtxos :: Array _)
        <> mustBeSignedBy adminPkh
        <> mustIncludeDatum bondedStateDatum
  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "closeBondedPoolContract: Cannot balance, reindex redeemers, attach/\
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "closeBondedPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash