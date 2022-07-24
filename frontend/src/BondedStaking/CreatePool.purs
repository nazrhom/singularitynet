module CreatePool (createBondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Monad
  ( Contract
  , liftContractAffM
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , logAesonInfo
  )
import Contract.PlutusData (PlutusData, Datum(Datum), toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction
  , balanceAndSignTx
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
import Plutus.Conversion (fromPlutusAddress)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Scripts.StateNFT (mkStateNFTPolicy)
import Settings
  ( bondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  )
import Types
  ( BondedPoolParams
  , BondedStakingDatum(StateDatum)
  , InitialBondedParams
  , StakingType(Bonded)
  )
import Utils (logInfo_, mkBondedPoolParams, repeatUntilConfirmed)

-- Sets up pool configuration, mints the state NFT and deposits
-- in the pool validator's address
createBondedPoolContract
  :: InitialBondedParams
  -> Contract ()
       { signedTx :: BalancedSignedTransaction
       , bondedPoolParams :: BondedPoolParams
       }
createBondedPoolContract ibp =
  repeatUntilConfirmed confirmationTimeout submissionAttempts
    do
      networkId <- getNetworkId
      adminPkh <- liftedM "createBondedPoolContract: Cannot get admin's pkh"
        ownPaymentPubKeyHash
      logInfo_ "createBondedPoolContract: Admin PaymentPubKeyHash" adminPkh
      -- Get the (Nami) wallet address
      adminAddr <-
        liftedM "createBondedPoolContract: Cannot get wallet Address"
          getWalletAddress
      logInfo_ "createBondedPoolContract: User Address"
        $ fromPlutusAddress networkId adminAddr
      -- Get utxos at the wallet address
      adminUtxos <- liftedM "createBondedPoolContract: Cannot get user Utxos"
        $ utxosAt adminAddr
      txOutRef <-
        liftContractM "createBondedPoolContract: Could not get head UTXO"
          $ fst
          <$> (head $ toUnfoldable $ unwrap adminUtxos)
      logInfo_ "createBondedPoolContract: Admin Utxos" adminUtxos
      -- Get the minting policy and currency symbol from the state NFT:
      statePolicy <- liftedE $ mkStateNFTPolicy Bonded txOutRef
      stateNftCs <-
        liftContractAffM
          "createBondedPoolContract: Cannot get CurrencySymbol from state NFT"
          $ scriptCurrencySymbol statePolicy
      -- Get the minting policy and currency symbol from the list NFT:
      listPolicy <- liftedE $ mkListNFTPolicy Bonded stateNftCs
      assocListCs <-
        liftContractAffM
          "createBondedPoolContract: Cannot get CurrencySymbol from state NFT"
          $ scriptCurrencySymbol listPolicy
      -- May want to hardcode this somewhere:
      tokenName <-
        liftContractM "createBondedPoolContract: Cannot create TokenName"
          bondedStakingTokenName
      -- We define the parameters of the pool
      let
        bondedPoolParams = mkBondedPoolParams adminPkh stateNftCs assocListCs
          ibp
      -- Get the bonding validator and hash
      validator <-
        liftedE' "createBondedPoolContract: Cannot create validator"
          $ mkBondedPoolValidator bondedPoolParams
      valHash <-
        liftContractAffM "createBondedPoolContract: Cannot hash validator"
          $ validatorHash validator
      let
        mintValue = singleton stateNftCs tokenName one
        poolAddr = scriptHashAddress valHash
      logInfo_ "createBondedPoolContract: BondedPool Validator's address"
        $ fromPlutusAddress networkId poolAddr
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

        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
            [ mustPayToScript valHash bondedStateDatum mintValue
            , mustMintValue mintValue
            , mustSpendPubKeyOutput txOutRef
            ]

      unattachedUnbalancedTx <-
        liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
      logAesonInfo unattachedUnbalancedTx
      -- `balanceAndSignTx` does the following:
      -- 1) Balance a transaction
      -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
      -- 3) Attach datums and redeemers to transaction.
      -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
      signedTx <-
        liftedM
          "createBondedPoolContract: Cannot balance, reindex redeemers, attach /\
          \datums redeemers and sign"
          $ balanceAndSignTx unattachedUnbalancedTx
      -- Return the transaction and the pool info for subsequent transactions
      pure { signedTx, bondedPoolParams }
