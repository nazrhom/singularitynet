module UnbondedStaking.CreatePool (createUnbondedPoolContract) where

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
  , mustMintValue
  , mustPayToScript
  , mustSpendPubKeyOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (scriptCurrencySymbol, singleton)
import Data.Array (head)
import Data.Map (toUnfoldable)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Scripts.StateNFT (mkStateNFTPolicy)
import Settings (unbondedStakingTokenName)
import Types (StakingType(Unbonded))
import UnbondedStaking.Types
 ( InitialUnbondedParams
 , UnbondedPoolParams
 , UnbondedStakingDatum(StateDatum)
 )
import Utils (logInfo_, mkUnbondedPoolParams)

-- Sets up pool configuration, mints the state NFT and deposits
-- in the pool validator's address
createUnbondedPoolContract
  :: InitialUnbondedParams -> Contract () UnbondedPoolParams
createUnbondedPoolContract iup = do
  networkId <- getNetworkId
  adminPkh <- liftedM "createUnbondedPoolContract: Cannot get admin's pkh"
    ownPaymentPubKeyHash
  logInfo_ "createUnbondedPoolContract: Admin PaymentPubKeyHash" adminPkh
  -- Get the (Nami) wallet address
  adminAddr <- liftedM "createUnbondedPoolContract: Cannot get wallet Address"
    getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "createUnbondedPoolContract: Cannot get user Utxos"
      $ utxosAt adminAddr
  txOutRef <-
    liftContractM "createUnbondedPoolContract: Could not get head UTXO"
      $ fst
      <$> (head $ toUnfoldable $ unwrap adminUtxos)
  logInfo_ "createUnbondedPoolContract: Admin Utxos" adminUtxos
  -- Get the minting policy and currency symbol from the state NFT:
  statePolicy <- liftedE $ mkStateNFTPolicy Unbonded txOutRef
  stateNftCs <-
    liftedM
      "createUnbondedPoolContract: Cannot get CurrencySymbol from /\
      \state NFT"
      $ scriptCurrencySymbol statePolicy
  -- Get the minting policy and currency symbol from the list NFT:
  listPolicy <- liftedE $ mkListNFTPolicy Unbonded stateNftCs
  assocListCs <-
    liftedM
      "createUnbondedPoolContract: Cannot get CurrencySymbol from /\
      \state NFT"
      $ scriptCurrencySymbol listPolicy
  -- May want to hardcode this somewhere:
  tokenName <-
    liftContractM "createUnbondedPoolContract: Cannot create TokenName"
      unbondedStakingTokenName
  -- We define the parameters of the pool
  let params = mkUnbondedPoolParams adminPkh stateNftCs assocListCs iup
  -- Get the bonding validator and hash
  validator <- liftedE' "createUnbondedPoolContract: Cannot create validator"
    $ mkUnbondedPoolValidator params
  valHash <- liftedM "createUnbondedPoolContract: Cannot hash validator"
    (validatorHash validator)
  let
    mintValue = singleton stateNftCs tokenName one
    poolAddr = validatorHashEnterpriseAddress networkId valHash
  logInfo_
    "createUnbondedPoolContract: UnbondedPool Validator's address" poolAddr
  let
    unbondedStateDatum = Datum $ toData $ StateDatum
      { maybeEntryName: Nothing
      , isOpen: true
      }

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.mintingPolicy statePolicy
      , ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap adminUtxos
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [ mustPayToScript valHash unbondedStateDatum mintValue
        , mustMintValue mintValue
        , mustSpendPubKeyOutput txOutRef
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
      "createUnbondedPoolContract: Cannot balance, reindex redeemers, attach /\
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "createUnbondedPoolContract: Transaction successfully submitted /\
    \with hash"
    $ byteArrayToHex
    $ unwrap transactionHash
  -- Return the pool info for subsequent transactions
  pure params
