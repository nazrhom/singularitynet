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
  , liftedE'
  , liftedM
  , logInfo'
  , throwContractError
  )
import Contract.Numeric.Natural (Natural, toBigInt)
import Contract.Numeric.Rational ((%))
import Contract.PlutusData
  ( PlutusData
  , Datum(Datum)
  , fromData
  , getDatumByHash
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput
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
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings (unbondedStakingTokenName)
import Types.Natural (fromBigInt')
import Types.Redeemer (Redeemer(Redeemer))
import Types.Scripts (ValidatorHash)
import UnbondedStaking.AdminUtils (calculateRewards, submitTransaction)
import UnbondedStaking.Types
  ( Entry(Entry)
  , UnbondedPoolParams(UnbondedPoolParams)
  , UnbondedStakingAction(AdminAct)
  , UnbondedStakingDatum(AssetDatum, EntryDatum, StateDatum)
  )
import Utils
  ( getUtxoWithNFT
  , mkOnchainAssocList
  , logInfo_
  , mkRatUnsafe
  , roundUp
  , splitByLength
  , toIntUnsafe
  )

-- | Deposits a certain amount in the pool
-- | If the `batchSize` is zero, then funds will be deposited to all users.
-- | Otherwise the transactions will be made in batches
-- | If the `depositList` is empty, then reward deposits will be made to all
-- | users. Otherwise only the users within the constraints/lookups will have
-- | rewards deposited.
-- | `callback` is used to run additional logic after a transaction has been
-- | submitted. The fuction must be of the following type signature:
-- |
-- | ```purescript
-- | callback :: Array
-- |   ( Tuple
-- |     (TxConstraints Unit Unit)
-- |     (ScriptLookups.ScriptLookups PlutusData)
-- |   )
-- |   -> Contract () Unit
-- | callback = ...
-- | ```
depositUnbondedPoolContract
  :: UnbondedPoolParams
  -> Natural
  -> Array
       ( Tuple
           (TxConstraints Unit Unit)
           (ScriptLookups.ScriptLookups PlutusData)
       )
  -> ( Array
         ( Tuple
             (TxConstraints Unit Unit)
             (ScriptLookups.ScriptLookups PlutusData)
         )
       -> Contract () Unit
     )
  -> Contract ()
       ( Array
           ( Tuple
               (TxConstraints Unit Unit)
               (ScriptLookups.ScriptLookups PlutusData)
           )
       )
depositUnbondedPoolContract
  params@
    ( UnbondedPoolParams
        { admin
        , nftCs
        , assocListCs
        }
    )
  batchSize
  depositList
  callback = do
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

        constraints :: TxConstraints Unit Unit
        constraints = mustBeSignedBy admin

        lookups :: ScriptLookups.ScriptLookups PlutusData
        lookups =
          ScriptLookups.validator validator
            <> ScriptLookups.unspentOutputs (unwrap unbondedPoolUtxos)
            <> ScriptLookups.unspentOutputs (unwrap adminUtxos)

        submitTxWithCallback
          :: Array
               ( Tuple
                   (TxConstraints Unit Unit)
                   (ScriptLookups.ScriptLookups PlutusData)
               )
          -> Contract ()
               ( Array
                   ( Tuple
                       (TxConstraints Unit Unit)
                       (ScriptLookups.ScriptLookups PlutusData)
                   )
               )
        submitTxWithCallback txBatch = do
          failedDeposits' <- submitTransaction constraints lookups txBatch
          callback failedDeposits'
          pure failedDeposits'
      -- Get list of users to deposit rewards too
      updateList <-
        if null depositList then
          traverse (mkEntryUpdateList params valHash) assocList
        else
          pure depositList
      -- Submit transaction with possible batching
      failedDeposits <-
        if batchSize == zero then do
          failedDeposits' <- submitTransaction constraints lookups updateList
          callback failedDeposits'
          pure failedDeposits'
        else do
          let updateBatches = splitByLength (toIntUnsafe batchSize) updateList
          failedDeposits' <- traverse submitTxWithCallback updateBatches
          pure $ mconcat failedDeposits'
      logInfo_
        "depositUnbondedPoolContract: Finished updating pool entries. /\
        \Entries with failed updates"
        failedDeposits
      pure failedDeposits
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

-- | Creates a constraint and lookups list for updating each user entry
mkEntryUpdateList
  :: UnbondedPoolParams
  -> ValidatorHash
  -> (ByteArray /\ TransactionInput /\ TransactionOutput)
  -> Contract ()
       ( Tuple (TxConstraints Unit Unit)
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
  logInfo_ "mkEntryUpdateList: datum hash" dHash
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
          "mkEntryUpdateList: Could not create token name for user"
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
        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
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
