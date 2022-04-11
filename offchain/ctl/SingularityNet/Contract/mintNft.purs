module SingularityNet.Contract.MintNft
  ( mintBondedStateNft
  ) where

import Contract.Prelude
import Contract.Address (getWalletAddress)
import Contract.Monad
  ( Contract
  , liftContractE'
  , liftContractM
  , liftedE'
  , liftedM
  )
import Contract.Prim.ByteArray (byteArrayFromString)
import Contract.PlutusData (PlutusData, Datum(Datum), toData, unitRedeemer)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustPayToOtherScript
  , mustMintValueWithRedeemer
  )
import Contract.Utxos (utxosAt)
import Contract.Value (mkSingletonValue', mkTokenName, scriptCurrencySymbol)
import SingularityNet.MintingPolicy (nftMintingPolicy)
import SingularityNet.Types (BondedStakingState(BondedStakingState))
import SingularityNet.Validator (bondedValidator)

mintBondedStateNft :: Contract () Unit
mintBondedStateNft = do
  -- Get the (Nami) wallet address
  userAddr <- liftedM "mintBondedStateNft: Cannot get wallet Address"
    getWalletAddress
  -- Get utxos at the wallet address
  userUtxos <-
    liftedM "mintBondedStateNft: Cannot get user Utxos" (utxosAt userAddr)
  -- Get the minting policy, the currency symbol and token name:
  policy <- liftedE' $ pure nftMintingPolicy
  curr <- liftedM "mintBondedStateNft: Cannot get CurrencySymbol"
    (scriptCurrencySymbol policy)
  -- May want to hardcode this somewhere:
  tokenName <- liftContractM "mintBondedStateNft: Cannot create TokenName"
    $ mkTokenName
    =<< byteArrayFromString "BondedStakingToken"
  mintValue <- liftContractM "mintBondedStateNft: Cannot create NFT Value"
    (mkSingletonValue' curr tokenName one)
  -- Get the bonding validator nad hash
  validator <- liftContractE' bondedValidator
  valHash <- liftedM "mintBondedStateNft: Cannot hash validator"
    (validatorHash validator)
  let
    bondedStateDatum = Datum $ toData $ BondedStakingState []

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.mintingPolicy policy
      , ScriptLookups.otherScript validator
      , ScriptLookups.unspentOutputs (unwrap userUtxos)
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [ mustPayToOtherScript valHash bondedStateDatum mintValue
        , mustMintValueWithRedeemer unitRedeemer mintValue
        ]

  unattachedBalancedTx <-
    liftedE' (ScriptLookups.mkUnbalancedTx lookup constraints)
  -- `balanceAndSignTx` does the following:
  -- 1) Balance a transaction
  -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
  -- 3) Attach datums and redeemers to transaction.
  -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
  BalancedSignedTransaction { signedTxCbor } <- liftedM
    "mintBondedStateNft: Cannot balance, reindex redeemers, attach datums/\
    \redeemers and sign"
    (balanceAndSignTx unattachedBalancedTx)
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  log $ "mintBondedStateNft: Transaction successfully submitted with hash: "
    <> show transactionHash