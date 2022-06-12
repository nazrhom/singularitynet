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
  , logInfo'
  , throwContractError
  )
import Contract.Numeric.Natural (toBigInt)
import Contract.Numeric.Rational (Rational, (%))
import Contract.PlutusData
  ( PlutusData
  , Datum(Datum)
  , fromData
  , getDatumByHash
  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , TransactionInput
  , TransactionOutput
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (mkTokenName, singleton)
import Control.Applicative (unless)
import Data.BigInt (BigInt)
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings (unbondedStakingTokenName)
import Types.Natural (fromBigInt')
import Types.Redeemer (Redeemer(Redeemer))
import Types.Scripts (ValidatorHash)
import UnbondedStaking.Types
  ( Entry(Entry)
  , UnbondedPoolParams(UnbondedPoolParams)
  , UnbondedStakingAction(AdminAct)
  , UnbondedStakingDatum(AssetDatum, EntryDatum, StateDatum)
  )
import Utils
  ( big
  , getUtxoWithNFT
  , mkOnchainAssocList
  , mkRatUnsafe
  , roundUp
  , logInfo_
  )

-- Deposits a certain amount in the pool
depositUnbondedPoolContract :: UnbondedPoolParams -> Contract () Unit
depositUnbondedPoolContract
  params@
    ( UnbondedPoolParams
        { admin
        , nftCs
        , assocListCs
        }
    ) = do
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
  -- Get the unbonded pool's utxo
  unbondedPoolUtxos <-
    liftedM
      "depositUnbondedPoolContract: Cannot get pool's utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "depositUnbondedPoolContract: Pool UTXOs" unbondedPoolUtxos
  tokenName <- liftContractM
    "depositUnbondedPoolContract: Cannot create TokenName"
    unbondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "depositUnbondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT unbondedPoolUtxos nftCs tokenName
  logInfo_ "depositUnbondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "depositUnbondedPoolContract: Could not get Pool UTXO's Datum Hash"
      (unwrap poolTxOutput).dataHash
  logInfo_ "depositUnbondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "depositUnbondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  unbondedStakingDatum :: UnbondedStakingDatum <-
    liftContractM
      "depositUnbondedPoolContract: Cannot extract NFT State datum"
      $ fromData (unwrap poolDatum)
  -- Update the association list
  case unbondedStakingDatum of
    -- Non-empty user list
    StateDatum { maybeEntryName: Just _, open: true } -> do
      logInfo'
        "depositUnbondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Just ..., open: true }"
      let
        assocList = mkOnchainAssocList assocListCs unbondedPoolUtxos
        updateContractList = (mkEntryUpdateList params valHash) <$> assocList
      updateList <- sequence updateContractList
      -- Concatinate constraints/lookups
      let
        constraintList = fst <$> updateList
        lookupList = snd <$> updateList

        constraints :: TxConstraints Unit Unit
        constraints =
          mustBeSignedBy admin
            <> mconcat (mconcat constraintList)

        lookups :: ScriptLookups.ScriptLookups PlutusData
        lookups =
          ScriptLookups.validator validator
            <> ScriptLookups.unspentOutputs (unwrap adminUtxos)
            <> ScriptLookups.unspentOutputs (unwrap unbondedPoolUtxos)
            <> mconcat lookupList
      -- Build transaction
      unattachedBalancedTx <-
        liftedE $ ScriptLookups.mkUnbalancedTx lookups constraints
      logInfo_
        "depositUnbondedPoolContract: unAttachedUnbalancedTx"
        unattachedBalancedTx
      BalancedSignedTransaction { signedTxCbor } <-
        liftedM
          "depositUnbondedPoolContract: Cannot balance, reindex redeemers, /\
          \attach datums redeemers and sign"
          $ balanceAndSignTx unattachedBalancedTx
      -- Submit transaction using Cbor-hex encoded `ByteArray`
      transactionHash <- submit signedTxCbor
      logInfo_
        "updateEntry: Transaction successfully submitted with hash"
        $ byteArrayToHex
        $ unwrap transactionHash
    -- Other error cases:
    StateDatum { maybeEntryName: Nothing, open: true } ->
      throwContractError
        "depositUnbondedPoolContract: There are no users in the pool to \
        \deposit rewards for"
    StateDatum { maybeEntryName: _, open: false } ->
      throwContractError
        "depositUnbondedPoolContract: Cannot deposit to a closed pool"
    _ ->
      throwContractError "depositUnbondedPoolContract: Datum incorrect type"

  logInfo' "depositUnbondedPoolContract: Finished updating pool entries"

-- | Calculates user awards according to spec formula
calculateRewards
  :: Rational
  -> BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> Contract () Rational
calculateRewards rewards totalRewards deposited newDeposit totalDeposited = do
  when (totalDeposited == zero) $
    throwContractError "calculateRewards: totalDeposited is zero"
  let
    lhs = mkRatUnsafe $ totalRewards % totalDeposited
    rhs = rewards + mkRatUnsafe (deposited % one)
    rhs' = rhs - mkRatUnsafe (newDeposit % one)
    f = rhs' * lhs
  when (f < zero) $ throwContractError
    "calculateRewards: invalid rewards amount"
  pure $ rewards + f

-- | Creates a constraint and lookups list for updating each user entry
mkEntryUpdateList
  :: UnbondedPoolParams
  -> ValidatorHash
  -> (ByteArray /\ TransactionInput /\ TransactionOutput)
  -> Contract ()
       ( Tuple (Array (TxConstraints Unit Unit))
           (ScriptLookups.ScriptLookups PlutusData)
       )
mkEntryUpdateList
  ( UnbondedPoolParams
      { increments
      , interest
      , unbondedAssetClass
      , assocListCs
      }
  )
  valHash
  (_ /\ txIn /\ txOut) = do
  -- Get the Entry datum of the old assoc. list element
  dHash <- liftContractM
    "mkEntryUpdateList: Could not get Entry Datum Hash"
    (unwrap txOut).dataHash
  logInfo_ "mkEntryUpdateList: datum hash " dHash
  listDatum <-
    liftedM
      "mkEntryUpdateList: Cannot get Entry's datum" $ getDatumByHash dHash
  unbondedListDatum :: UnbondedStakingDatum <-
    liftContractM
      "mkEntryUpdateList: Cannot extract NFT State datum"
      $ fromData (unwrap listDatum)
  -- The get the entry datum
  case unbondedListDatum of
    EntryDatum { entry } -> do
      let e = unwrap entry
      calculatedRewards <-
        calculateRewards
          e.rewards
          e.totalRewards
          e.deposited
          e.newDeposit
          e.totalDeposited
      -- Get the token name for the user by hashing
      assocListTn <-
        liftContractM
          "mkEntryUpdateList: Could not create token name for user`"
          $ mkTokenName e.key
      -- Update the entry datum
      let
        updatedRewards = roundUp calculatedRewards
        updatedTotalDeposited = e.deposited + updatedRewards
        incrementsRat = mkRatUnsafe (toBigInt increments % one)
        updatedTotalRewards = updatedTotalDeposited *
          (roundUp (interest * incrementsRat))
        -- Datum and redeemer creation
        entryDatum = Datum $ toData $ EntryDatum
          { entry: Entry $ e
              { newDeposit = zero
              , rewards = mkRatUnsafe (updatedRewards % one)
              , totalRewards = updatedTotalRewards
              , totalDeposited = updatedTotalDeposited
              }
          }
        valRedeemer = Redeemer $ toData $ AdminAct
          { totalRewards: fromBigInt' $ updatedTotalRewards
          , totalDeposited: fromBigInt' $ updatedTotalDeposited
          }
        -- Build asset datum and value types
        assetDatum = Datum $ toData AssetDatum
        assetParams = unwrap unbondedAssetClass
        assetCs = assetParams.currencySymbol
        assetTn = assetParams.tokenName
        depositValue = singleton assetCs assetTn updatedRewards
        entryValue = singleton assocListCs assocListTn one

        -- Build constraints and lookups
        constraints :: Array (TxConstraints Unit Unit)
        constraints =
          [ mustPayToScript valHash assetDatum depositValue
          , mustPayToScript valHash entryDatum entryValue
          , mustSpendScriptOutput txIn valRedeemer
          ]
      entryDatumLookup <-
        liftContractM
          "mkEntryUpdateList: Could not create state datum lookup"
          $ ScriptLookups.datum entryDatum
      pure (constraints /\ entryDatumLookup)
    _ -> throwContractError
      "mkEntryUpdateList: Datum not Entry constructor"
