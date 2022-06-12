module UnbondedStaking.ClosePool (closeUnbondedPoolContract) where

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
import Contract.Numeric.Rational (Rational, (%))
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData
  , Redeemer(Redeemer)
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
  , mustIncludeDatum
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (mkTokenName, singleton)
import Data.BigInt (BigInt)
import Data.Map (toUnfoldable)
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings (unbondedStakingTokenName)
import Types.Scripts (ValidatorHash)
import UnbondedStaking.Types
  ( Entry(Entry)
  , UnbondedPoolParams(UnbondedPoolParams)
  , UnbondedStakingAction(CloseAct)
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

closeUnbondedPoolContract :: UnbondedPoolParams -> Contract () Unit
closeUnbondedPoolContract
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
  userPkh <- liftedM "closeUnbondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "closeUnbondedPoolContract: Admin is not current user"
  logInfo_ "closeUnbondedPoolContract: Admin PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  AddressWithNetworkTag { address: adminAddr } <-
    liftedM "depositUnbondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "depositUnbondedPoolContract: Cannot get user Utxos" $
      utxosAt adminAddr
  -- Get the unbonded pool validator and hash
  validator <- liftedE' "closeUnbondedPoolContract: Cannot create validator"
    $ mkUnbondedPoolValidator params
  valHash <- liftContractM "closeUnbondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "closeUnbondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "closeUnbondedPoolContract: Pool address"
    $ fromPlutusType (networkId /\ poolAddr)
  -- Get the unbonded pool's utxo
  unbondedPoolUtxos <-
    liftedM
      "closeUnbondedPoolContract: Cannot get pool's utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "closeUnbondedPoolContract: Pool UTXOs" unbondedPoolUtxos
  tokenName <- liftContractM
    "closeUnbondedPoolContract: Cannot create TokenName"
    unbondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "closeUnbondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT unbondedPoolUtxos nftCs tokenName
  logInfo_ "closeUnbondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "closeUnbondedPoolContract: Could not get Pool UTXO's Datum Hash"
      (unwrap poolTxOutput).dataHash
  logInfo_ "closeUnbondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "closeUnbondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  unbondedStakingDatum :: UnbondedStakingDatum <-
    liftContractM
      "closeUnbondedPoolContract: Cannot extract NFT State datum"
      $ fromData (unwrap poolDatum)
  -- Update the association list
  constraints /\ lookups <- case unbondedStakingDatum of
    -- Non-empty user list
    StateDatum { maybeEntryName: Just key, open: true } -> do
      logInfo'
        "closeUnbondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Just ..., open: true }"
      let
        assocList = mkOnchainAssocList assocListCs unbondedPoolUtxos
        stateTokenValue = singleton nftCs tokenName one
        stateDatum = Datum $ toData $ StateDatum
          { maybeEntryName: Just key
          , open: false
          }
      updateList <- traverse (mkEntryUpdateList params valHash) assocList
      unbondedStateDatumLookup <-
        liftContractM
          "closeUnbondedPoolContract: Could not create state datum lookup"
          $ ScriptLookups.datum stateDatum
      -- Concatinate constraints/lookups
      let
        constraintList = fst <$> updateList
        lookupList = snd <$> updateList

        constraints :: TxConstraints Unit Unit
        constraints =
          mustBeSignedBy admin
            <> mustIncludeDatum poolDatum
            <> mustPayToScript valHash stateDatum stateTokenValue
            <> mconcat (mconcat constraintList)

        lookups :: ScriptLookups.ScriptLookups PlutusData
        lookups =
          ScriptLookups.validator validator
            <> ScriptLookups.unspentOutputs (unwrap adminUtxos)
            <> ScriptLookups.unspentOutputs (unwrap unbondedPoolUtxos)
            <> unbondedStateDatumLookup
            <> mconcat lookupList
      logInfo' "closeUnbondedPoolContract: Built tx constraints and lookups"
      pure $ constraints /\ lookups
    -- Closing pool with no users
    StateDatum { maybeEntryName: Nothing, open: true } -> do
      logInfo'
        "closeUnbondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Nothing, open: true }"
      -- Update datum and create redeemer
      let
        stateTokenValue = singleton nftCs tokenName one
        stateDatum = Datum $ toData $ StateDatum
          { maybeEntryName: Nothing
          , open: false
          }
        redeemer = Redeemer $ toData CloseAct
      unbondedStateDatumLookup <-
        liftContractM
          "closeUnbondedPoolContract: Could not create state datum lookup"
          $ ScriptLookups.datum stateDatum
      -- Bulid constraints/lookups
      let
        constraints :: TxConstraints Unit Unit
        constraints =
          -- Spend all UTXOs to return to Admin along with state/assets
          foldMap
            (flip mustSpendScriptOutput redeemer <<< fst)
            (toUnfoldable $ unwrap unbondedPoolUtxos :: Array _)
            <> mustBeSignedBy admin
            <> mustIncludeDatum poolDatum
            <> mustPayToScript valHash stateDatum stateTokenValue

        lookups :: ScriptLookups.ScriptLookups PlutusData
        lookups = mconcat
          [ ScriptLookups.validator validator
          , ScriptLookups.unspentOutputs $ unwrap unbondedPoolUtxos
          , unbondedStateDatumLookup
          ]
      logInfo' "closeUnbondedPoolContract: Built tx constraints and lookups"
      pure $ constraints /\ lookups
    -- Other error cases:
    StateDatum { maybeEntryName: _, open: false } ->
      throwContractError
        "closeUnbondedPoolContract: Pool is already closed"
    _ ->
      throwContractError "closeUnbondedPoolContract: Datum incorrect type"
  -- Build transaction
  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookups constraints
  logInfo_
    "closeUnbondedPoolContract: unAttachedUnbalancedTx"
    unattachedBalancedTx
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "closeUnbondedPoolContract: Cannot balance, reindex redeemers, attach /\
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "closeUnbondedPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash
  logInfo' "closeUnbondedPoolContract: Successfully closed pool"

-- | Calculates user awards according to spec formula
calculateRewards
  :: Rational
  -> BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> Contract () Rational
calculateRewards rewards totalRewards deposited newDeposit totalDeposited = do
  when (totalDeposited == zero) $ throwContractError
    "calculateRewards: totalDeposited is zero"
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
      { unbondedAssetClass
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
        -- Datum and redeemer creation
        entryDatum = Datum $ toData $ EntryDatum
          { entry: Entry $ e
              { rewards = mkRatUnsafe (updatedRewards % one)
              , open = false
              }
          }
        valRedeemer = Redeemer $ toData CloseAct
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
