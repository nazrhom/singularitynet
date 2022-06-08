module UserWithdraw (userWithdrawBondedPoolContract) where

import Contract.Prelude hiding (length)
import Prelude

import Contract.Address (AddressWithNetworkTag(AddressWithNetworkTag), getNetworkId, getWalletAddress, ownPaymentPubKeyHash, scriptHashAddress)
import Contract.Monad (Contract, liftContractM, liftedE, liftedE', liftedM, logInfo', throwContractError)
import Contract.Numeric.Natural (Natural, toBigInt)
import Contract.PlutusData (PlutusData, Datum(Datum), fromData, getDatumByHash, toData)
import Contract.Prim.ByteArray (ByteArray(..), byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (BalancedSignedTransaction(BalancedSignedTransaction), TransactionInput(..), TransactionOutput(..), balanceAndSignTx, submit)
import Contract.TxConstraints (TxConstraints, mustBeSignedBy, mustMintCurrencyWithRedeemer, mustMintValueWithRedeemer, mustPayToPubKey, mustPayToScript, mustSpendScriptOutput)
import Contract.Utxos (UtxoM(..), utxosAt)
import Contract.Value (Value, mkTokenName, singleton)
import Control.Applicative (unless)
import Data.Array (catMaybes, head)
import Data.BigInt (BigInt)
import Data.Map (Map, toUnfoldable, fromFoldable)
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedStakingTokenName)
import Types (BondedPoolParams(BondedPoolParams), BondedStakingAction(..), BondedStakingDatum(..), BurningAction(..), Entry(..), ListAction(..), MintingAction(MintHead), StakingType(Bonded))
import Types.Rational (Rational, denominator, numerator, reduce, (%))
import Types.Redeemer (Redeemer(Redeemer))
import Utils (findAssocElem, getUtxoWithNFT, hashPkh, mkOnchainAssocList, logInfo_)

-- Deposits a certain amount in the pool
userWithdrawBondedPoolContract :: BondedPoolParams -> Contract () Unit
userWithdrawBondedPoolContract
  params@
    ( BondedPoolParams
        { minStake
        , maxStake
        , bondedAssetClass
        , nftCs
        , assocListCs
        }
    )
  = do
  ---- FETCH BASIC INFORMATION ----
  -- Get network ID
  networkId <- getNetworkId
  -- Get own public key hash and compute hashed version
  userPkh <- liftedM "userWithdrawBondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  let hashedUserPkh = hashPkh userPkh
  logInfo_ "userWithdrawBondedPoolContract: User's PaymentPubKeyHash" userPkh
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
  validator <- liftedE' "userWithdrawBondedPoolContract: Cannot create validator"
    $ mkBondedPoolValidator params
  valHash <- liftContractM "userWithdrawBondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "userWithdrawBondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "userWithdrawBondedPoolContract: Pool address"
    $ fromPlutusType (networkId /\ poolAddr)
  -- Get the bonded pool's utxo
  bondedPoolUtxos <- liftedM "userWithdrawBondedPoolContract: Cannot get pool's\
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
  logInfo' "userWithdrawBondedPoolContract: Getting bonded assets in \
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
  -- Build useful values for later
  let
    assetDatum = Datum $ toData AssetDatum
    stateTokenValue = singleton nftCs tokenName one
    burnEntryValue = singleton assocListCs assocListTn (-one)
    assetParams = unwrap bondedAssetClass
    assetCs = assetParams.currencySymbol
    assetTn = assetParams.tokenName
    assocList = mkOnchainAssocList params bondedPoolUtxos
  ---- BUILD CONSTRAINTS AND LOOKUPS ----
  constraints /\ lookup <- case headEntry of
    Nothing -> throwContractError "userWithdrawBondedPoolContract: no entries \
      \in the pool, expected at least one"
    Just headKey -> do
      logInfo'
        "userWithdrawBondedPoolContract: Found the head entry successfully"
      case compare hashedUserPkh headKey of
        -- If hashedUserPkh < headKey, we are trying to withdraw a non-existent
        --  entry
        LT -> throwContractError "userWithdrawBondedPoolContract: entry key < \
          \head key (non existent)"
        -- If hashedUserPkh == key, we are trying to withdraw the first entry of
        --  the list
        EQ -> do
          logInfo' "userWithdrawBondedPoolContract: Compare EQ"
          (_ /\ txIn /\ txOut) <-
            liftContractM
              "userWithdrawBondedPoolContract: Cannot \
              \extract head from Assoc. List - this should be impossible"
              $ head assocList
          -- Get the datum of the head entry and the key of the new head
          oldHeadEntry <- unwrap <$> getEntryDatumFromOutput txOut
          let newHeadKey :: Maybe ByteArray
              newHeadKey = oldHeadEntry.next
          -- Get amount to withdraw
          let rewards :: Rational
              rewards = oldHeadEntry.rewards 
              rewardsRounded :: BigInt
              rewardsRounded = numerator rewards / denominator rewards
              withdrawnAmt :: BigInt
              withdrawnAmt = oldHeadEntry.staked + rewardsRounded
              withdrawnVal :: Value
              withdrawnVal = singleton assetCs assetTn withdrawnAmt
          -- Build updated state
          let newState :: Datum
              newState = Datum <<< toData $
                StateDatum { maybeEntryName: newHeadKey }
          -- Build validator redeemer
          let valRedeemer = Redeemer <<< toData $
               WithdrawAct {
                 stakeHolder: userPkh ,
                 burningAction: BurnHead txIn
               }
          -- Build minting policy redeemer
          let mintRedeemer = Redeemer $ toData $ ListRemove $ BurnHead txIn
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
                , mustSpendScriptOutput txIn valRedeemer
                , mustMintValueWithRedeemer mintRedeemer burnEntryValue
                , mustPayToPubKey userPkh withdrawnVal
                ]

            lookup :: ScriptLookups.ScriptLookups PlutusData
            lookup = mconcat
              [ ScriptLookups.validator validator
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
          mintingAction
            /\ { firstInput, secondInput }
            /\ { firstOutput, secondOutput }
            /\ { secondKey } <-
            liftContractM
              "userWithdrawBondedPoolContract: Cannot get position in Assoc. List"
              $ findAssocElem assocList hashedUserPkh

          when (isJust mintingAction) $
            throwContractError "userWithdrawBondedPoolContract: Could not find\
              \ entry in list"
          
          -- Build validator redeemer
          --let
          --  valRedeemer = Redeemer $ toData $ StakeAct
          --    { stakeAmount: amt
          --    , stakeHolder: userPkh
          --    , mintingAction
          --    }

          ---- Get the Entry datum of the old assoc. list (first) element
          --dHash <- liftContractM
          --  "userWithdrawBondedPoolContract: Could not get Entry Datum Hash"
          --  (unwrap firstOutput).dataHash
          --logInfo_ "userWithdrawBondedPoolContract:" dHash
          --firstListDatum <-
          --  liftedM
          --    "userWithdrawBondedPoolContract: Cannot get \
          --    \Entry's  datum" $ getDatumByHash dHash
          --firstBondedListDatum :: BondedStakingDatum <-
          --  liftContractM
          --    "userWithdrawBondedPoolContract: Cannot extract Assoc. List datum"
          --    $ fromData (unwrap firstListDatum)

          ---- Constraints for the first element.
          --firstConstraints /\ firstLookups <- case firstBondedListDatum of
          --  EntryDatum { entry } -> do
          --    let e = unwrap entry
          --    firstEntryDatum <-
          --      if isJust mintingAction then
          --        -- MintInBetween and MintEnd are the same here
          --        -- a new middle entry is created so update next
          --        pure $ Datum $ toData $ EntryDatum
          --          { entry: Entry $ e
          --              { next = Just hashedUserPkh
          --              }
          --          }
          --      else do -- depositing/updating at the first entry
          --        let updateDeposited = e.deposited + amtBigInt
          --        unless
          --          ( toBigInt minStake <= updateDeposited
          --              && updateDeposited
          --              <= toBigInt maxStake
          --          )
          --          $ throwContractError
          --              "userWithdrawBondedPoolContract: Stake amount outside of \
          --              \min/max range"
          --        pure $ Datum $ toData $ EntryDatum
          --          { entry: Entry $ e
          --              { newDeposit = e.newDeposit + amtBigInt
          --              , deposited = updateDeposited
          --              }
          --          }
          --    firstEntryDatumLookup <-
          --      liftContractM
          --        "userWithdrawBondedPoolContract: Could not create state datum \
          --        \lookup"
          --        $ ScriptLookups.datum firstEntryDatum
          --    let
          --      constr = mconcat
          --        [ mustPayToScript valHash firstEntryDatum entryValue
          --        , mustSpendScriptOutput firstInput valRedeemer
          --        ]
          --      -- We add validator at the end. If we are minting i.e. when
          --      -- mintingAction is "Just", we include those in
          --      -- `middleConstraints` and `middleLookups` below
          --      lu = firstEntryDatumLookup
          --    pure $ constr /\ lu
          --  _ -> throwContractError
          --    "userWithdrawBondedPoolContract: Datum not \
          --    \Entry constructor"

          ---- Constraints for the potential middle element.
          --middleConstraints /\ middleLookups <-
          --  if isJust mintingAction then do -- a genuine new entry
          --    unless (minStake <= amt && amt <= maxStake)
          --      $ throwContractError
          --          "userWithdrawBondedPoolContract: Stake amount outside of \
          --          \min/max range"
          --    -- Inbetween mint - this should not fail because we have `Just`
          --    ma <- liftContractM
          --      "userWithdrawBondedPoolContract: Could not get \
          --      \minting action"
          --      mintingAction
          --    let
          --      -- Minting a new Entry
          --      mintRedeemer = Redeemer $ toData $ ListInsert ma

          --      entryDatum = Datum $ toData $ Entry
          --        { key: hashedUserPkh
          --        , newDeposit: amtBigInt
          --        , deposited: amtBigInt
          --        , staked: zero
          --        , rewards: zero
          --        , next: secondKey -- points to original second key
          --        }

          --    entryDatumLookup <-
          --      liftContractM
          --        "userWithdrawBondedPoolContract: Could not create state datum \
          --        \lookup"
          --        $ ScriptLookups.datum entryDatum
          --    let
          --      constr = mconcat
          --        [ mustMintValueWithRedeemer mintRedeemer entryValue
          --        , mustPayToScript valHash entryDatum entryValue
          --        ]
          --      lu = mconcat
          --        [ ScriptLookups.mintingPolicy listPolicy
          --        , entryDatumLookup
          --        ]
          --    pure $ constr /\ lu
          --  else pure $ mempty /\ mempty

          ---- Get the constraints for the second assoc. list element
          --lastConstraints /\ lastLookups <- case secondOutput, secondInput of
          --  Nothing, Nothing -> pure $ mempty /\ mempty
          --  Just so, Just si -> do --
          --    dh <- liftContractM
          --      "userWithdrawBondedPoolContract: Could not get Entry Datum Hash"
          --      (unwrap so).dataHash
          --    logInfo_ "userWithdrawBondedPoolContract:" dh
          --    secondListDatum <-
          --      liftedM
          --        "userWithdrawBondedPoolContract: Cannot \
          --        \get Entry's datum" $ getDatumByHash dh
          --    secondBondedListDatum :: BondedStakingDatum <-
          --      liftContractM
          --        "userWithdrawBondedPoolContract: Cannot extract NFT State datum"
          --        $ fromData (unwrap secondListDatum)

          --    -- Unchanged in the case
          --    lastEntryDatum <- case secondBondedListDatum of
          --      EntryDatum { entry } ->
          --        pure $ Datum $ toData $ EntryDatum { entry }
          --      _ -> throwContractError
          --        "userWithdrawBondedPoolContract: Datum not \
          --        \Entry constructor"

          --    lastEntryDatumLookup <-
          --      liftContractM
          --        "userWithdrawBondedPoolContract: Could not create state datum \
          --        \lookup"
          --        $ ScriptLookups.datum lastEntryDatum
          --    let
          --      constr = mconcat
          --        [ mustPayToScript valHash lastEntryDatum entryValue
          --        , mustSpendScriptOutput si valRedeemer
          --        ]
          --      lu = mconcat
          --        [ ScriptLookups.mintingPolicy listPolicy
          --        , lastEntryDatumLookup
          --        ]
          --    pure $ constr /\ lu
          --  _, _ -> throwContractError
          --    "userWithdrawBondedPoolContract: Datum not\
          --    \Entry constructor"
          --pure
          --  $ mconcat
          --      [ firstConstraints
          --      , middleConstraints
          --      , lastConstraints
          --      , mustBeSignedBy userPkh
          --      ]
          --  /\
          --    mconcat
          --      [ ScriptLookups.validator validator
          --      , ScriptLookups.unspentOutputs $ unwrap userUtxos
          --      , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
          --      , firstLookups
          --      , middleLookups
          --      , lastLookups
          --      ]
          undefined

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
    
-- This function filters all the asset UTxOs from a `UtxoM`
getBondedAssetUtxos :: forall (r :: Row Type) . UtxoM -> Contract r UtxoM
getBondedAssetUtxos utxos = do
  assetUtxos <- catMaybes <$> for utxoAssocList \utxo@(_ /\ txOutput) -> do
    datumHash <- liftContractM "getAssetUtxos: could not get datum hash"
      (unwrap txOutput).dataHash
    datum <-
      liftContractM "getAssetUtxos: could not get datum"
      =<< getDatumByHash datumHash
    bondedDatum :: BondedStakingDatum <-
      liftContractM "getAssetUtxos: could not parse datum as a bonded staking \
        \datum" $ fromData (unwrap datum)
    case bondedDatum of
      AssetDatum -> pure $ Just utxo
      _ -> pure Nothing
  pure $ UtxoM $ fromFoldable assetUtxos
  where utxoAssocList :: Array (TransactionInput /\ TransactionOutput)
        utxoAssocList = toUnfoldable $ unwrap utxos
        
-- Get entry datum from transaction output
getEntryDatumFromOutput ::
  forall r . TransactionOutput -> Contract r Entry
getEntryDatumFromOutput txOut = do
  bondedDatum <- getBondedDatum txOut
  case bondedDatum of
    EntryDatum {entry: e} -> pure e
    _ -> throwContractError "getEntryDatumFromOutput: datum is not of Entry \
          \type"
          
-- Get state datum from transaction output
getStateDatumFromOutput ::
  forall r . TransactionOutput -> Contract r (Maybe ByteArray)
getStateDatumFromOutput txOut = do
  bondedDatum <- getBondedDatum txOut
  case bondedDatum of
    StateDatum { maybeEntryName: key } -> pure key
    _ -> throwContractError "getStateDatumFromOutput: datum is not of State \
      \type"
          
-- Get a bonded datum from a transaction output
getBondedDatum ::
  forall r . TransactionOutput -> Contract r BondedStakingDatum
getBondedDatum = 
  liftContractM "getBondedDatum: could not parse datum as bonded staking datum"
    <<< fromData <<< unwrap
  <=< liftContractM "getBondedDatum: could not get datum"
  <=< getDatumByHash
  <=< liftContractM "getBondedDatum: could not get datum hash"
    <<< _.dataHash <<< unwrap
    
