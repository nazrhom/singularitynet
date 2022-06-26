module CreatePool (createBondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( AddressWithNetworkTag(AddressWithNetworkTag)
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
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
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Scripts.StateNFT (mkStateNFTPolicy)
import Settings (bondedStakingTokenName)
import Types
  ( BondedPoolParams
  , BondedStakingDatum(StateDatum)
  , InitialBondedParams
  , StakingType(Bonded)
  )
import Utils (logInfo_, mkBondedPoolParams)

-- Sets up pool configuration, mints the state NFT and deposits
-- in the pool validator's address
createBondedPoolContract :: InitialBondedParams -> Contract () BondedPoolParams
createBondedPoolContract ibp = do
  networkId <- getNetworkId
  adminPkh <- liftedM "createBondedPoolContract: Cannot get admin's pkh"
    ownPaymentPubKeyHash
  logInfo_ "createBondedPoolContract: Admin PaymentPubKeyHash" adminPkh
  -- Get the (Nami) wallet address
  AddressWithNetworkTag { address: adminAddr } <-
    liftedM "createBondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  logInfo_ "createBondedPoolContract: User Address"
    $ fromPlutusType (networkId /\ adminAddr)
  -- Get utxos at the wallet address
  adminUtxos <- liftedM "createBondedPoolContract: Cannot get user Utxos"
    $ utxosAt adminAddr
  txOutRef <- liftContractM "createBondedPoolContract: Could not get head UTXO"
    $ fst
    <$> (head $ toUnfoldable $ unwrap adminUtxos)
  logInfo_ "createBondedPoolContract: Admin Utxos" adminUtxos
  -- Get the minting policy and currency symbol from the state NFT:
  statePolicy <- liftedE $ mkStateNFTPolicy Bonded txOutRef
  stateNftCs <-
    liftContractM
      "createBondedPoolContract: Cannot get CurrencySymbol from state NFT"
      $ scriptCurrencySymbol statePolicy
  -- Get the minting policy and currency symbol from the list NFT:
  listPolicy <- liftedE $ mkListNFTPolicy Bonded stateNftCs
  assocListCs <-
    liftContractM
      "createBondedPoolContract: Cannot get CurrencySymbol from state NFT"
      $ scriptCurrencySymbol listPolicy
  -- May want to hardcode this somewhere:
  tokenName <-
    liftContractM "createBondedPoolContract: Cannot create TokenName"
      bondedStakingTokenName
  -- We define the parameters of the pool
  let params = mkBondedPoolParams adminPkh stateNftCs assocListCs ibp
  -- Get the bonding validator and hash
  validator <-
    liftedE' "createBondedPoolContract: Cannot create validator"
      $ mkBondedPoolValidator params
  valHash <- liftContractM "createBondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  let
    mintValue = singleton stateNftCs tokenName one
    poolAddr = scriptHashAddress valHash
  logInfo_ "createBondedPoolContract: BondedPool Validator's address"
    $ fromPlutusType (networkId /\ poolAddr)
  let
    -- We initalize the pool with no head entry and a pool size of 100_000_000
    bondedStateDatum = Datum $ toData $ StateDatum
      { maybeEntryName: Nothing
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
        [ mustPayToScript valHash bondedStateDatum mintValue
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
      "createBondedPoolContract: Cannot balance, reindex redeemers, attach /\
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "createBondedPoolContract: Transaction successfully submitted /\
    \with hash"
    $ byteArrayToHex
    $ unwrap transactionHash
  -- Return the pool info for subsequent transactions
  pure params
