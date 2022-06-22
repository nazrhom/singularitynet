module UserWithdraw (userWithdrawBondedPoolContract) where

import Contract.Prelude hiding (length)

import BondedStaking.TimeUtils (getWithdrawingTime)
import Contract.Address (AddressWithNetworkTag(AddressWithNetworkTag), getNetworkId, getWalletAddress, ownPaymentPubKeyHash, ownStakePubKeyHash, scriptHashAddress)
import Contract.Monad (Contract, liftContractM, liftedE, liftedE', liftedM, logInfo', throwContractError)
import Contract.PlutusData (PlutusData, Datum(Datum), fromData, getDatumByHash, toData)
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (BalancedSignedTransaction(BalancedSignedTransaction), TransactionInput, TransactionOutput, balanceAndSignTx, submit)
import Contract.TxConstraints (TxConstraints, mustBeSignedBy, mustMintValueWithRedeemer, mustPayToPubKeyAddress, mustPayToScript, mustSpendScriptOutput, mustValidateIn)
import Contract.Utxos (UtxoM(UtxoM), utxosAt)
import Contract.Value (Value, mkTokenName, singleton)
import Data.Array (catMaybes, head)
import Data.BigInt (BigInt)
import Data.Map as Map
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedStakingTokenName)
import Types (BondedPoolParams(BondedPoolParams), BondedStakingAction(WithdrawAct), BondedStakingDatum(AssetDatum, EntryDatum, StateDatum), BurningAction(BurnHead, BurnOther), Entry(Entry), ListAction(ListRemove), StakingType(Bonded))
import Types.Rational (Rational, denominator, numerator)
import Types.Redeemer (Redeemer(Redeemer))
import Utils (findRemoveOtherElem, getAssetsToConsume, mkAssetUtxosConstraints, getUtxoWithNFT, hashPkh, logInfo_, mkOnchainAssocList)

-- Deposits a certain amount in the pool
userWithdrawBondedPoolContract :: BondedPoolParams -> Contract () Unit
userWithdrawBondedPoolContract
  params@
    ( BondedPoolParams
        { bondedAssetClass
        , nftCs
        , assocListCs
        }
    ) = do
  ---- FETCH BASIC INFORMATION ----
  -- Get network ID
  networkId <- getNetworkId
  -- Get own public key hash and compute hashed version
  userPkh <- liftedM "userWithdrawBondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  let hashedUserPkh = hashPkh userPkh
  logInfo_ "userWithdrawBondedPoolContract: User's PaymentPubKeyHash" userPkh
  -- Get own staking hash
  userStakingPubKeyHash <-
    liftedM
      "userWithdrawnBondedPoolContract: Cannot get\
      \ user's staking pub key hash" $
      ownStakePubKeyHash
  -- Get the (Nami) wallet address
  AddressWithNetworkTag { address: userAddr } <-
    liftedM "userWithdrawBondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  userUtxos <-
    liftedM "userWithdrawBondedPoolContract: Cannot get user Utxos"
      $ utxosAt userAddr
  ---- FETCH POOL DATA ----
  -- Get the bonded pool validator and hash
  validator <-
    liftedE' "userWithdrawBondedPoolContract: Cannot create validator"
      $ mkBondedPoolValidator params
  valHash <-
    liftContractM "userWithdrawBondedPoolContract: Cannot hash validator"
      $ validatorHash validator
  logInfo_ "userWithdrawBondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "userWithdrawBondedPoolContract: Pool address"
    $ fromPlutusType (networkId /\ poolAddr)
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM
      "userWithdrawBondedPoolContract: Cannot get pool's\
      \ utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "userWithdrawBondedPoolContract: Pool UTxOs" bondedPoolUtxos
  tokenName <- liftContractM
    "userWithdrawBondedPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "userWithdrawBondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT bondedPoolUtxos nftCs tokenName
  logInfo' "userWithdrawBondedPoolContract: Getting head entry of the pool..."
  headEntry <- getStateDatumFromOutput poolTxOutput
  logInfo_ "userWithdrawBondedPoolContract: Head entry of the pool" headEntry
  -- Get asset UTxOs in bonded pool
  logInfo'
    "userWithdrawBondedPoolContract: Getting bonded assets in \
    \the pool..."
  bondedAssetUtxos <- getBondedAssetUtxos bondedPoolUtxos
  logInfo_ "userWithdrawnBondedPoolContract: Bonded Asset UTxOs"
    bondedAssetUtxos
  -- Get the minting policy and currency symbol from the list NFT:
  listPolicy <- liftedE $ mkListNFTPolicy Bonded nftCs
  -- Get the token name for the user by hashing
  assocListTn <-
    liftContractM
      "userWithdrawBondedPoolContract: Could not create token name for user`"
      $ mkTokenName hashedUserPkh

  -- Get the staking range to use
  logInfo' "userWithdrawBondedPoolContract: Getting staking range..."
  { currTime, range: txRange } <- getWithdrawingTime params
  logInfo_ "Current time: " $ show currTime
  logInfo_ "TX Range" txRange

  -- Build useful values for later
  let
    stateTokenValue = singleton nftCs tokenName one
    mintEntryValue = singleton assocListCs assocListTn one
    burnEntryValue = singleton assocListCs assocListTn (-one)
    assetParams = unwrap bondedAssetClass
    assetCs = assetParams.currencySymbol
    assetTn = assetParams.tokenName
    assetDatum = Datum $ toData AssetDatum
    assocList = mkOnchainAssocList assocListCs bondedPoolUtxos
  ---- BUILD CONSTRAINTS AND LOOKUPS ----
  constraints /\ lookup <- case headEntry of
    Nothing -> throwContractError
      "userWithdrawBondedPoolContract: no entries \
      \in the pool, expected at least one"
    Just headKey -> do
      logInfo'
        "userWithdrawBondedPoolContract: Found the head entry successfully"
      case compare hashedUserPkh headKey of
        -- If hashedUserPkh < headKey, we are trying to withdraw a non-existent
        --  entry
        LT -> throwContractError
          "userWithdrawBondedPoolContract: entry key < \
          \head key (non existent)"
        -- If hashedUserPkh == key, we are trying to withdraw the first entry of
        --  the list
        EQ -> do
          logInfo' "userWithdrawBondedPoolContract: Compare EQ"
          _ /\ entryInput /\ entryOutput <-
            liftContractM
              "userWithdrawBondedPoolContract: Cannot \
              \extract head from Assoc. List - this should be impossible"
              $ head assocList
          -- Get the datum of the head entry and the key of the new head
          logInfo'
            "userWithdrawBondedPoolContract: getting datum of entry to\
            \consume (head)..."
          oldHeadEntry <- unwrap <$> getEntryDatumFromOutput entryOutput
          logInfo_ "userWithdrawBondedPoolContract: entry to consume"
            oldHeadEntry
          let
            newHeadKey :: Maybe ByteArray
            newHeadKey = oldHeadEntry.next

            -- Get amount to withdraw
            rewards :: Rational
            rewards = oldHeadEntry.rewards

            rewardsRounded :: BigInt
            rewardsRounded = numerator rewards / denominator rewards

            withdrawnAmt :: BigInt
            withdrawnAmt = oldHeadEntry.staked + rewardsRounded

            withdrawnVal :: Value
            withdrawnVal = singleton assetCs assetTn withdrawnAmt
          -- Calculate assets to consume and change that needs to be returned
          -- to the pool
          consumedAssetUtxos /\ totalSpentAmt <-
            liftContractM
              "userWithdrawBondedPoolContract: Cannot get asset \
              \UTxOs to consume" $
              getAssetsToConsume bondedAssetClass withdrawnAmt bondedAssetUtxos
          let
            changeValue :: Value
            changeValue =
              singleton
                (unwrap bondedAssetClass).currencySymbol
                (unwrap bondedAssetClass).tokenName
                $ totalSpentAmt
                - withdrawnAmt

            -- Build updated state
            newState :: Datum
            newState = Datum <<< toData $
              StateDatum { maybeEntryName: newHeadKey }

            -- Build validator redeemer
            valRedeemer = Redeemer <<< toData $
              WithdrawAct
                { stakeHolder: userPkh
                , burningAction: BurnHead poolTxInput entryInput
                }

            -- Build minting policy redeemer
            mintRedeemer = Redeemer $ toData $ ListRemove $ BurnHead poolTxInput
              entryInput
          -- New state lookup
          stateDatumLookup <-
            liftContractM
              "userWithdrawBondedPoolContract: Could not create state datum lookup"
              $ ScriptLookups.datum newState

          let
            constraints :: TxConstraints Unit Unit
            constraints =
              mconcat
                [ mustBeSignedBy userPkh
                , mustSpendScriptOutput poolTxInput valRedeemer
                , mustSpendScriptOutput entryInput valRedeemer
                , mkAssetUtxosConstraints consumedAssetUtxos valRedeemer
                , mustMintValueWithRedeemer mintRedeemer burnEntryValue
                , mustPayToScript valHash newState stateTokenValue
                , mustPayToScript valHash assetDatum changeValue
                , mustPayToPubKeyAddress userPkh userStakingPubKeyHash
                    withdrawnVal
                , mustValidateIn txRange
                ]

            lookup :: ScriptLookups.ScriptLookups PlutusData
            lookup = mconcat
              [ ScriptLookups.validator validator
              , ScriptLookups.mintingPolicy listPolicy
              , ScriptLookups.unspentOutputs $ unwrap userUtxos
              , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
              , stateDatumLookup
              ]
          pure $ constraints /\ lookup
        -- If hashedUserPkh > key, we find the wanted entry in the list and
        --  withdraw its respective funds
        GT -> do
          -- The hashed key is greater than so we must look at the assoc. list
          -- in more detail
          logInfo' "userWithdrawBondedPoolContract: Compare GT"
          { firstInput, secondInput }
            /\ { firstOutput, secondOutput }
            /\ _ <-
            liftContractM
              "userWithdrawBondedPoolContract: Cannot get position in Assoc. List"
              $ findRemoveOtherElem assocList hashedUserPkh

          -- Get the entry datum of the previous entry
          logInfo'
            "userWithdrawBondedPoolContract: getting datum of previous\
            \entry..."
          prevEntry <- unwrap <$> getEntryDatumFromOutput firstOutput
          logInfo_ "userWithdrawBondedPoolContract: entry to consume" prevEntry

          -- Get the entry datum of the entry to consume
          logInfo'
            "userWithdrawBondedPoolContract: getting datum of entry to\
            \ burn..."
          burnEntry <- unwrap <$> getEntryDatumFromOutput secondOutput
          logInfo_ "userWithdrawBondedPoolContract: entry to consume" burnEntry

          -- Get amount to withdraw
          let
            rewards :: Rational
            rewards = burnEntry.rewards

            rewardsRounded :: BigInt
            rewardsRounded = numerator rewards / denominator rewards

            withdrawnAmt :: BigInt
            withdrawnAmt = burnEntry.staked + rewardsRounded

            withdrawnVal :: Value
            withdrawnVal = singleton assetCs assetTn withdrawnAmt

          -- Calculate assets to consume and change that needs to be returned
          -- to the pool
          consumedAssetUtxos /\ totalSpentAmt <-
            liftContractM
              "userWithdrawBondedPoolContract: Cannot get asset \
              \UTxOs to consume" $
              getAssetsToConsume bondedAssetClass withdrawnAmt bondedAssetUtxos
          let
            changeValue :: Value
            changeValue =
              singleton
                (unwrap bondedAssetClass).currencySymbol
                (unwrap bondedAssetClass).tokenName
                $ totalSpentAmt
                - withdrawnAmt

            -- Build updated previous entry and its lookup
            prevEntryUpdated = Datum $ toData $ EntryDatum
              { entry: Entry $ prevEntry
                  { next = burnEntry.next
                  }
              }
          prevEntryDatumLookup <-
            liftContractM
              "userWithdrawBondedPoolContract: Could not create updated prev \
              \ entry datum lookup"
              $ ScriptLookups.datum prevEntryUpdated

          -- Build validator redeemer
          let
            valRedeemer = Redeemer <<< toData $
              WithdrawAct
                { stakeHolder: userPkh
                , burningAction: BurnOther firstInput secondInput
                }

            -- Build minting policy redeemer
            mintRedeemer = Redeemer $ toData $ ListRemove $ BurnOther firstInput
              secondInput

            constraints :: TxConstraints Unit Unit
            constraints =
              mconcat
                [ mustBeSignedBy userPkh
                , mustSpendScriptOutput firstInput valRedeemer
                , mustSpendScriptOutput secondInput valRedeemer
                , mkAssetUtxosConstraints consumedAssetUtxos valRedeemer
                , mustMintValueWithRedeemer mintRedeemer burnEntryValue
                , mustPayToScript valHash prevEntryUpdated mintEntryValue
                , mustPayToScript valHash assetDatum changeValue
                , mustPayToPubKeyAddress userPkh userStakingPubKeyHash
                    withdrawnVal
                , mustValidateIn txRange
                ]

            lookup :: ScriptLookups.ScriptLookups PlutusData
            lookup = mconcat
              [ ScriptLookups.validator validator
              , ScriptLookups.mintingPolicy listPolicy
              , ScriptLookups.unspentOutputs $ unwrap userUtxos
              , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
              , prevEntryDatumLookup
              ]

          pure $ constraints /\ lookup

  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  logInfo_
    "userWithdrawBondedPoolContract: unAttachedUnbalancedTx"
    unattachedBalancedTx
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "userWithdrawBondedPoolContract: Cannot balance, reindex redeemers, attach \
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "userWithdrawBondedPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash

-- | This function filters all the asset UTxOs from a `UtxoM`
getBondedAssetUtxos :: forall (r :: Row Type). UtxoM -> Contract r UtxoM
getBondedAssetUtxos utxos = do
  assetUtxos <- catMaybes <$> for utxoAssocList \utxo@(_ /\ txOutput) -> do
    datumHash <- liftContractM "getAssetUtxos: could not get datum hash"
      (unwrap txOutput).dataHash
    datum <-
      liftContractM "getAssetUtxos: could not get datum"
        =<< getDatumByHash datumHash
    bondedDatum :: BondedStakingDatum <-
      liftContractM
        "getAssetUtxos: could not parse datum as a bonded staking \
        \datum" $ fromData (unwrap datum)
    case bondedDatum of
      AssetDatum -> pure $ Just utxo
      _ -> pure Nothing
  pure <<< UtxoM $ Map.fromFoldable assetUtxos
  where
  utxoAssocList :: Array (TransactionInput /\ TransactionOutput)
  utxoAssocList = Map.toUnfoldable $ unwrap utxos

-- | Get entry datum from transaction output
getEntryDatumFromOutput
  :: forall (r :: Row Type). TransactionOutput -> Contract r Entry
getEntryDatumFromOutput txOut = do
  bondedDatum <- getBondedDatum txOut
  case bondedDatum of
    EntryDatum { entry: e } -> pure e
    _ -> throwContractError
      "getEntryDatumFromOutput: datum is not of Entry \
      \type"

-- | Get state datum from transaction output
getStateDatumFromOutput
  :: forall (r :: Row Type). TransactionOutput -> Contract r (Maybe ByteArray)
getStateDatumFromOutput txOut = do
  bondedDatum <- getBondedDatum txOut
  case bondedDatum of
    StateDatum { maybeEntryName: key } -> pure key
    _ -> throwContractError
      "getStateDatumFromOutput: datum is not of State \
      \type"

-- | Get a bonded datum from a transaction output
getBondedDatum
  :: forall (r :: Row Type). TransactionOutput -> Contract r BondedStakingDatum
getBondedDatum =
  liftContractM "getBondedDatum: could not parse datum as bonded staking datum"
    <<< fromData
    <<< unwrap
    <=< liftContractM "getBondedDatum: could not get datum"
    <=< getDatumByHash
    <=< liftContractM "getBondedDatum: could not get datum hash"
    <<< _.dataHash
    <<< unwrap
