module UnbondedStaking.DepositPool (depositUnbondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( AddressWithNetworkTag(AddressWithNetworkTag)
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
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
import Contract.Time (always)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustPayToScript
  , mustValidateIn
  )
import Contract.Utxos (utxosAt)
import Contract.Value (singleton)
import Control.Applicative (unless)
import Data.BigInt (fromString) as BigInt
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import UnbondedStaking.Types
  ( UnbondedPoolParams(UnbondedPoolParams)
  -- , UnbondedStakingAction(AdminAct)
  , UnbondedStakingDatum(AssetDatum)
  )
import Utils (big, logInfo_)

-- Deposits a certain amount in the pool
depositUnbondedPoolContract :: UnbondedPoolParams -> Contract () Unit
depositUnbondedPoolContract
  params@(UnbondedPoolParams { admin }) = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "depositUnbondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "depositUnbondedPoolContract: Admin is not current user"
  logInfo_ "depositUnbondedPoolContract: Admin PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  AddressWithNetworkTag { address: adminAddr } <-
    liftedM "depositUnbondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "depositUnbondedPoolContract: Cannot get user Utxos" $
      utxosAt adminAddr

  -- Get the unbonded pool validator and hash
  validator <- liftedE' "depositUnbondedPoolContract: Cannot create validator"
    $ mkUnbondedPoolValidator params
  valHash <- liftContractM "depositUnbondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "depositUnbondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "depositUnbondedPoolContract: Pool address"
    $ fromPlutusType (networkId /\ poolAddr)

  -- tempBigInt <-
  --   liftContractM
  --     "depositUnbondedPoolContract: TEMPORARY - cannot\
  --     \ convert String to BigInt" $ BigInt.fromString "4000000"
  -- Create the datums and their ScriptLookups
  let
    -- This is the datum of the UTXO that will hold the rewards
    assetDatum = Datum $ toData AssetDatum
    assetParams = unwrap (unwrap params).unbondedAssetClass
    assetCs = assetParams.currencySymbol
    assetTn = assetParams.tokenName
    depositValue = singleton assetCs assetTn (big 4_000_000) --tempBigInt

    -- We build the redeemer. The size does not change because there are no
    -- user stakes. It doesn't make much sense to deposit if there wasn't a
    -- change in the total amount of stakes (and accrued rewards). This will
    -- change when user staking is added
    -- redeemerData = toData $ AdminAct
    --   { totalRewards: nat 10_000_000
    --   , totalDeposited: nat 10_000_000
    --   }
    -- redeemer = Redeemer redeemerData

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap adminUtxos
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [
          -- Deposit rewards in a separate UTXO
          mustPayToScript valHash assetDatum depositValue
        , mustBeSignedBy admin

        -- TODO: Validate transaction within current/next adminLength
        , mustValidateIn always

        -- TODO: Find and spend entry UTXOs with updated values
        -- TODO: Implement batching algorithm for UTXOs (and fragmenting
        -- across multiple Tx's if required due to size limits)
        -- TODO: Split asset datums into separate per-user UTXOs
        ]
  dh <- liftContractM "depositUnbondedPoolContract: Cannot Hash AssetDatum"
    $ datumHash assetDatum
  logInfo_ "depositUnbondedPoolContract: DatumHash of AssetDatum" dh

  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  logInfo_
    "depositUnbondedPoolContract: unAttachedUnbalancedTx"
    unattachedBalancedTx
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "depositUnbondedPoolContract: Cannot balance, reindex redeemers, attach /\
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "depositUnbondedPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash