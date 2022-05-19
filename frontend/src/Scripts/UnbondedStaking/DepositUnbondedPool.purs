module UnbondedStaking.DepositUnbondedPool (depositUnbondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
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
import Contract.PlutusData (PlutusData, Datum(Datum), toData, datumHash)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , balanceTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (singleton)
import Control.Applicative (unless)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (unbondedStakingTokenName)
import Types.Redeemer (Redeemer(Redeemer))
import UnbondedStaking.Types
  ( UnbondedPoolParams(UnbondedPoolParams)
  , BondedStakingAction(AdminAct)
  , BondedStakingDatum(AssetDatum, StateDatum)
  )
import Utils (big, getUtxoWithNFT, logInfo_, nat)

-- Deposits a certain amount in the pool
depositUnbondedPoolContract :: UnbondedPoolParams -> Contract () Unit
depositUnbondedPoolContract params@(BondedPoolParams { admin, nftCs }) = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "depositUnbondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "depositUnbondedPoolContract: Admin \
    \is not current user"
  logInfo_ "depositUnbondedPoolContract: Admin PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  adminAddr <- liftedM "depositUnbondedPoolContract: Cannot get wallet Address"
    getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "depositUnbondedPoolContract: Cannot get user Utxos" $
      utxosAt adminAddr
  -- Get the unbonded pool validator and hash
  validator <- liftedE' "depositUnbondedPoolContract: Cannot create validator"
    $ mkUnbondedPoolValidator params
  valHash <- liftedM "depositUnbondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "depositUnbondedPoolContract: validatorHash" valHash
  let poolAddr = validatorHashEnterpriseAddress networkId valHash
  logInfo_ "depositUnbondedPoolContract: Pool address" poolAddr
  -- Get the unbonded pool's utxo
  unbondedPoolUtxos <-
    liftedM
      "depositUnbondedPoolContract: Cannot get pool's utxos at pool address" $
      utxosAt poolAddr
  logInfo_ "depositUnbondedPoolContract: Pool UTXOs" unbondedPoolUtxos
  tokenName <- liftContractM
    "depositUnbondedPoolContract: Cannot create TokenName"
    unbondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "depositUnbondedPoolContract: Cannot get state utxo" $
      getUtxoWithNFT unbondedPoolUtxos nftCs tokenName
  logInfo_ "depositUnbondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "depositUnbondedPoolContract: Could not get Pool UTXO's Datum Hash"
      (unwrap poolTxOutput).dataHash
  logInfo_ "depositUnbondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  -- Create the datums and their ScriptLookups
  let
    -- We can hardcode the state for now. We should actually fetch the datum
    -- from Ogmios, update it properly and then submit it
    unbondedStateDatum = Datum $ toData $ StateDatum
      { maybeEntryName: Nothing
      , isOpen: true
      }
    -- This is the datum of the UTXO that will hold the rewards
    assetDatum = Datum $ toData $ AssetDatum

  unbondedStateDatumLookup <-
    liftContractM
      "depositUnbondedPoolContract: Could not create state datum lookup"
      =<< ScriptLookups.datum unbondedStateDatum
  let
    assetParams = unwrap (unwrap params).bondedAssetClass
    assetCs = assetParams.currencySymbol
    assetTn = assetParams.tokenName
    stateTokenValue = singleton nftCs tokenName one
    depositValue = singleton assetCs assetTn (big 2)
    scriptAddr = validatorHashEnterpriseAddress networkId valHash
  logInfo_
    "depositUnbondedPoolContract: UnbondedPool Validator's address"
    scriptAddr
  let
    -- We build the redeemer. The size does not change because there are no
    -- user stakes. It doesn't make much sense to deposit if there wasn't a
    -- change in the total amount of stakes (and accrued rewards). This will
    -- change when user staking is added
    redeemerData = toData $ AdminAct
      { totalRewards: nat 100_000_000
      , totalDeposited: nat 100_000_000
      }
    redeemer = Redeemer redeemerData

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap adminUtxos
      , ScriptLookups.unspentOutputs $ unwrap unbondedPoolUtxos
      , unbondedStateDatumLookup
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [
          -- Update the pool's state
          mustPayToScript valHash unbondedStateDatum stateTokenValue
        -- Deposit rewards in a separate UTXO
        , mustPayToScript valHash assetDatum depositValue
        , mustBeSignedBy admin
        , mustSpendScriptOutput poolTxInput redeemer
        ]
  dh <- liftedM "depositUnbondedPoolContract: Cannot Hash AssetDatum" $
    datumHash
    assetDatum
  dh' <- liftedM "depositUnbondedPoolContract: Cannot Hash UnbondedStateDatum"
    $ datumHash
      unbondedStateDatum
  logInfo_ "depositUnbondedPoolContract: DatumHash of AssetDatum" dh
  logInfo_ "depositUnbondedPoolContract: DatumHash of UnbondedStateDatum" dh'
  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  logInfo_
    "depositUnbondedPoolContract: unAttachedUnbalancedTx"
    unattachedBalancedTx
  let unbalancedTx = (unwrap unattachedBalancedTx).unbalancedTx
  balancedTx <- liftedE $ balanceTx unbalancedTx
  logInfo_ "depositUnbondedPoolContract: balancedTx" balancedTx
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "depositUnbondedPoolContract: Cannot balance, reindex redeemers, attach/\
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "depositUnbondedPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash
