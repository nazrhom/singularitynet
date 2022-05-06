module DepositPool (depositPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , validatorHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractM, liftedE, liftedE', liftedM)
import Contract.PlutusData (PlutusData, Datum(Datum), toData)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (adaSymbol, adaToken, singleton)
import Data.Array (head)
import Data.Map (toUnfoldable)
import Scripts.BondedPoolValidator (mkBondedPoolValidator)
import Settings (hardCodedParams)
import Types
  ( BondedStakingAction(AdminAct)
  , BondedStakingDatum(StateDatum)
  , PoolInfo(PoolInfo)
  )
import Types.Redeemer (Redeemer(Redeemer))
import Utils (big, nat, logInfo_)

-- Deposits a certain amount in the pool
depositPoolContract :: PoolInfo -> Contract () Unit
depositPoolContract (PoolInfo { stateNftCs, assocListCs, poolAddr }) = do
  -- Fetch information related to the pool
  -- Get network ID and admin's PKH
  logInfo_ "Pool address" poolAddr
  networkId <- getNetworkId
  adminPkh <- liftedM "depositPoolContract: Cannot get admin's pkh"
    ownPaymentPubKeyHash
  logInfo_ "Admin PaymentPubKeyHash" adminPkh
  -- Get the (Nami) wallet address
  adminAddr <- liftedM "depositPoolContract: Cannot get wallet Address"
    getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "depositPoolContract: Cannot get user Utxos" $ utxosAt adminAddr
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM "depositPoolContract: Cannot get pool's utxos at pool address" $
      utxosAt poolAddr
  poolTxInput <-
    liftContractM "depositPoolContract: Cannot get head Utxo for bonded pool"
      $ fst
      <$> (head $ toUnfoldable $ unwrap bondedPoolUtxos)
  logInfo_ "Pool's UTXO" poolTxInput
  -- We define the parameters of the pool
  params <- liftContractM "depositPoolContract: Failed to create parameters" $
    hardCodedParams adminPkh stateNftCs assocListCs
  logInfo_ "toData Pool Parameters" $ toData params
  -- Get the bonded pool validator and hash
  validator <- liftedE' "depositPoolContract: Cannot create validator" $
    mkBondedPoolValidator params
  valHash <- liftedM "depositPoolContract: Cannot hash validator"
    $ validatorHash validator
  let
    depositValue = singleton adaSymbol adaToken $ big 5
    scriptAddr = validatorHashEnterpriseAddress networkId valHash
  logInfo_ "BondedPool Validator's address" scriptAddr
  let
    -- We can hardcode the state for now. We should actually fetch the datum
    -- from Ogmios, update it properly and then submit it
    bondedStateDatum = Datum $ toData $ StateDatum
      { maybeEntryName: Nothing
      }
    -- We build the redeemer
    redeemerData = toData $ AdminAct { sizeLeft: nat 100 }
    redeemer = Redeemer redeemerData

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap adminUtxos
      , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [ mustPayToScript valHash bondedStateDatum depositValue
        , mustSpendScriptOutput poolTxInput redeemer
        ]
  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  -- `balanceAndSignTx` does the following:
  -- 1) Balance a transaction
  -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
  -- 3) Attach datums and redeemers to transaction.
  -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "depositPoolContract: Cannot balance, reindex redeemers, attach datums/\
      \redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_ "depositPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex $ unwrap transactionHash
