module CreatePool (createBondedPoolContract, getBondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( Address
  , Bech32String
  , PaymentPubKeyHash(..)
  , addressToBech32
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Log (logAesonInfo)
import Contract.Monad (Contract, liftContractM, liftedE, liftedE', liftedM)
import Contract.PlutusData (PlutusData, Datum(Datum), toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (BalancedSignedTransaction, balanceAndSignTx)
import Contract.TxConstraints
  ( TxConstraints
  , mustMintValue
  , mustSpendPubKeyOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (scriptCurrencySymbol, singleton)
import Data.Array (head)
import Data.Bitraversable (rtraverse)
import Data.Map (toUnfoldable, fromFoldable)
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
import Utils
  ( logInfo_
  , mkBondedPoolParams
  , repeatUntilConfirmed
  , mustPayToScript
  )

-- Sets up pool configuration, mints the state NFT and deposits
-- in the pool validator's address
createBondedPoolContract
  :: InitialBondedParams
  -> Contract ()
       { signedTx :: BalancedSignedTransaction
       , bondedPoolParams :: BondedPoolParams
       , address :: Bech32String
       }
createBondedPoolContract initialBondedParams =
  repeatUntilConfirmed confirmationTimeout submissionAttempts
    do
      -- Get the bonded pool parameters from initial parameters
      bondedPoolParams /\ address <- getBondedParams initialBondedParams
      let
        bpp = unwrap bondedPoolParams
        ibp = unwrap initialBondedParams
      -- Get the bonded pool's validator and hash
      validator <-
        liftedE' "createBondedPoolContract: Cannot create validator"
          $ mkBondedPoolValidator bondedPoolParams
      -- Get the bonded pool's minting policy and token name
      statePolicy <-
        liftedE' "createBondedPoolContract: Cannot create state policy"
          $ mkStateNFTPolicy Bonded
          $ fst ibp.nftUtxo
      tokenName <-
        liftedM "createBondedPoolContract: Cannot obtain bonded token name"
          (pure bondedStakingTokenName)
      let
        valHash = validatorHash validator
        mintValue = singleton bpp.nftCs tokenName one
      addressBech <- addressToBech32 $ address
      logInfo_ "createBondedPoolContract: BondedPool Validator's address"
        address
      let
        -- We initalize the pool with no head entry and a pool size of 100_000_000
        bondedStateDatum = Datum $ toData $ StateDatum
          { maybeEntryName: Nothing
          }

        lookup :: ScriptLookups.ScriptLookups PlutusData
        lookup = mconcat
          [ ScriptLookups.mintingPolicy statePolicy
          , ScriptLookups.validator validator
          , ScriptLookups.unspentOutputs $ fromFoldable [ ibp.nftUtxo ]
          ]

        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
            [ mustPayToScript valHash bondedStateDatum mintValue
            , mustMintValue mintValue
            , mustSpendPubKeyOutput $ fst ibp.nftUtxo
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
      pure { signedTx, bondedPoolParams, address: addressBech }

-- | Creates the final Bonded pool params and address from the initial
-- arguments provided. It's used both for creating the pool in the blockchain
-- and re-creating the pool object that is used in the SDK.
getBondedParams
  :: InitialBondedParams -> Contract () (BondedPoolParams /\ Address)
getBondedParams initialBondedParams = do
  let ibp = unwrap initialBondedParams
  -- Get own PKH
  adminPkh <- liftedM "getBondedParams: Cannot get admin's pkh"
    ownPaymentPubKeyHash
  -- Get the minting policy and currency symbol from the state NFT:
  statePolicy <- liftedE $ mkStateNFTPolicy Bonded $ fst ibp.nftUtxo
  stateNftCs <-
    liftContractM
      "getBondedParams: Cannot get CurrencySymbol from state NFT"
      $ scriptCurrencySymbol statePolicy
  -- Get the minting policy and currency symbol from the list NFT:
  listPolicy <- liftedE $ mkListNFTPolicy Bonded stateNftCs
  assocListCs <-
    liftContractM
      "getBondedParams: Cannot get CurrencySymbol from state NFT"
      $ scriptCurrencySymbol listPolicy
  -- We create the parameters of the pool
  let
    bondedPoolParams = mkBondedPoolParams adminPkh stateNftCs assocListCs
      initialBondedParams
  -- Obtain validator's address
  validator <-
    liftedE' "getBondedParams: Cannot create validator"
      $ mkBondedPoolValidator bondedPoolParams
  let address = scriptHashAddress $ validatorHash validator
  pure $ bondedPoolParams /\ address

-- | Retrieves the bonded pool's arguments and address from the
-- initial arguments provided to its constructor. It does *not* create a new
-- pool in the blockchain.
getBondedPoolContract
  :: InitialBondedParams
  -> Contract ()
       { bondedPoolParams :: BondedPoolParams
       , address :: Bech32String
       }
getBondedPoolContract ibp = do
  -- Obtain bonded pool parameters and address in Bech32 format
  bondedPoolParams /\ address <- rtraverse addressToBech32 =<< getBondedParams
    ibp
  -- Validate the given pool is actually present in the network

  pure { bondedPoolParams, address }

