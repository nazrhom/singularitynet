module UserStake (userStakeBondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( AddressWithNetworkTag(AddressWithNetworkTag)
  , PaymentPubKeyHash(PaymentPubKeyHash)
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Hashing (blake2b256Hash)
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , logInfo'
  , throwContractError
  )
import Contract.Numeric.Natural (Natural, toBigInt)
import Contract.PlutusData
  ( PlutusData
  , Datum(Datum)
  , datumHash
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
  , TransactionOutput(TransactionOutput)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustMintValueWithRedeemer
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Contract.Utxos (UtxoM(UtxoM), utxosAt)
import Contract.Value
  ( TokenName
  , flattenValue
  , getTokenName
  , mkTokenName
  , singleton
  )
import Control.Alternative (guard)
import Control.Applicative (unless)
import Data.Array (filter, head, last, length, mapMaybe, partition, sortBy)
import Data.Map (toUnfoldable)
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedStakingTokenName)
import Serialization.Hash (ed25519KeyHashToBytes)
import Types
  ( BondedStakingAction(StakeAct)
  , BondedStakingDatum (AssetDatum, EntryDatum, StateDatum)
  , BondedPoolParams(BondedPoolParams)
  , Entry(Entry)
  , ListAction(ListInsert)
  , MintingAction(MintEnd, MintHead, MintInBetween)
  , StakingType(Bonded)
  )
import Types.Redeemer (Redeemer(Redeemer))
import Utils (big, getUtxoWithNFT, hashPkh, logInfo_, nat)

mkOnchainAssocList
  :: BondedPoolParams
  -> UtxoM
  -> Array (ByteArray /\ TransactionInput /\ TransactionOutput)
mkOnchainAssocList (BondedPoolParams { assocListCs }) (UtxoM utxos) =
  sortBy compareBytes $ mapMaybe getAssocListUtxos $ toUnfoldable utxos
  where
  getAssocListUtxos
    :: TransactionInput /\ TransactionOutput
    -> Maybe (ByteArray /\ TransactionInput /\ TransactionOutput)
  getAssocListUtxos utxo@(_ /\ (TransactionOutput txOutput)) = do
    let val = flattenValue txOutput.amount
    cs /\ tn /\ amt <- head val
    guard (length val == one && cs == assocListCs && amt == one)
    pure $ (unwrap $ getTokenName tn) /\ utxo

-- | Find the assoc list element to update or insert. This can be optimised
-- | if we compare pairs and exit early of course. But we'll do this for
-- | simplicity. THIS MUST BE USED ON A SORTED LIST, i.e. with
-- | `mkOnchainAssocList`
findAssocElem
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutput)
  -> ByteArray
  -> Maybe (Maybe MintingAction
    /\ { firstInput :: TransactionInput, secondInput :: Maybe TransactionInput }
    /\ { firstOutput :: TransactionOutput, secondOutput :: Maybe TransactionOutput }
    /\ { firstKey :: ByteArray, secondKey :: Maybe ByteArray })
findAssocElem assocList hashedKey = do
  -- The list should findAssocElem assocList hashedKey = do be sorted so no need to resort
  let { no, yes } = partition (fst >>> (>=) hashedKey) assocList
  bytesL /\ txInputL /\ txOutputL <- last yes
  -- If we're at the last element, it must be an end stake
  if length no == zero then do
    -- Workout whether it's an initial deposit
    let mintingAction = if bytesL == hashedKey then Nothing else Just $ MintEnd txInputL
    pure $ mintingAction /\ { firstInput: txInputL, secondInput: Nothing } /\ { firstOutput: txOutputL, secondOutput: Nothing } /\ { firstKey: bytesL, secondKey: Nothing}
  -- Otherwise, it is an inbetween stake:
  else do
    bytesH /\ txInputH /\ txOutputH <- head no
    let mintingAction = if bytesL == hashedKey then Nothing else Just $ MintInBetween txInputL txInputH
    pure $ mintingAction /\ { firstInput: txInputL, secondInput: Just txInputH } /\ { firstOutput: txOutputL, secondOutput: Just txOutputH } /\ { firstKey: bytesL, secondKey: Just bytesH }

compareBytes :: ByteArray /\ _ -> ByteArray /\ _ -> Ordering
compareBytes (bytes /\ _) (bytes' /\ _) = compare bytes bytes'

-- Deposits a certain amount in the pool
userStakeBondedPoolContract :: BondedPoolParams -> Natural -> Contract () Unit
userStakeBondedPoolContract params@(BondedPoolParams
  { minStake
  , maxStake
  , admin
  , bondedAssetClass
  , nftCs
  , assocListCs
  }) amt = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "userStakeBondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  logInfo_ "userStakeBondedPoolContract: User's PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  AddressWithNetworkTag { address: userAddr } <-
    liftedM "userStakeBondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  userUtxos <-
    liftedM "userStakeBondedPoolContract: Cannot get user Utxos"
      $ utxosAt userAddr
  -- Get the bonded pool validator and hash
  validator <- liftedE' "userStakeBondedPoolContract: Cannot create validator"
    $ mkBondedPoolValidator params
  valHash <- liftContractM "userStakeBondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "userStakeBondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "userStakeBondedPoolContract: Pool address"
    $ fromPlutusType (networkId /\ poolAddr)
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM
      "userStakeBondedPoolContract: Cannot get pool's utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "userStakeBondedPoolContract: Pool UTXOs" bondedPoolUtxos
  tokenName <- liftContractM
    "userStakeBondedPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "userStakeBondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT bondedPoolUtxos nftCs tokenName
  logInfo_ "userStakeBondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "userStakeBondedPoolContract: Could not get Pool UTXO's Datum Hash"
      (unwrap poolTxOutput).dataHash
  logInfo_ "userStakeBondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "userStakeBondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  bondedStakingDatum :: BondedStakingDatum <- liftContractM
    "userStakeBondedPoolContract: Cannot extract NFT State datum"
    $ fromData (unwrap poolDatum)
  let hashedUserPkh = hashPkh userPkh
  assocListTn <-
    liftContractM
      "userStakeBondedPoolContract: Could not create token name for user`"
      $ mkTokenName hashedUserPkh
  constraints /\ lookup <- case bondedStakingDatum of
    StateDatum { maybeEntryName: Nothing } -> do
      logInfo' "userStakeBondedPoolContract: StateDatum is \
        \StateDatum { maybeEntryName: Nothing }"
      -- If the state UTXO has nothing, it is a head deposit, spending the state
      -- UTXO
      unless (minStake <= amt && amt <= maxStake)
        $ throwContractError
        "userStakeBondedPoolContract: Stake amount outside of min/max range"
      let
        mh = MintHead poolTxInput
        -- Minting a new Entry
        valRedeemer = Redeemer $ toData $ StakeAct
          { stakeAmount: amt
          , stakeHolder: userPkh
          , mintingAction: Just mh
          }
        mintRedeemer = Redeemer $ toData $ ListInsert mh
        -- Updated bonded state datum
        bondedStateDatum = Datum $ toData $ StateDatum
          { maybeEntryName: Just hashedUserPkh
          }
        amtBigInt = toBigInt amt
        -- The new Entry
        entryDatum = Datum $ toData $ Entry
          { key: hashedUserPkh
          , newDeposit: amtBigInt
          , deposited: amtBigInt
          , staked: zero
          , rewards: zero
          , next: Nothing
          }
        assetDatum = Datum $ toData AssetDatum
        stateTokenValue = singleton nftCs tokenName one
        assetParams = unwrap bondedAssetClass
        assetCs = assetParams.currencySymbol
        assetTn = assetParams.tokenName
        stakeValue = singleton assetCs assetTn amtBigInt
        entryValue = singleton assocListCs assocListTn one

      -- Get the minting policy and currency symbol from the list NFT:
      listPolicy <- liftedE $ mkListNFTPolicy Bonded nftCs

      bondedStateDatumLookup <-
        liftContractM
          "userStakeBondedPoolContract: Could not create state datum lookup"
          $ ScriptLookups.datum bondedStateDatum
      let
        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
            [ mustMintValueWithRedeemer mintRedeemer entryValue
            , mustPayToScript valHash bondedStateDatum stateTokenValue
            , mustPayToScript valHash assetDatum stakeValue
            , mustPayToScript valHash entryDatum entryValue
            , mustBeSignedBy userPkh
            , mustSpendScriptOutput poolTxInput valRedeemer
            ]

        lookup :: ScriptLookups.ScriptLookups PlutusData
        lookup = mconcat
          [ ScriptLookups.mintingPolicy listPolicy
          , ScriptLookups.validator validator
          , ScriptLookups.unspentOutputs $ unwrap userUtxos
          , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
          , bondedStateDatumLookup
          ]
      pure $ constraints /\ lookup
    StateDatum { maybeEntryName: Just key } -> do
      logInfo' "userStakeBondedPoolContract: StateDatum is \
        \StateDatum { maybeEntryName: Just ... }"
      let assocList = mkOnchainAssocList params bondedPoolUtxos
      -- If hashedUserPkh < key, we have a head deposit, spending the state utxo
      -- If hashedUserPkh == key, this is a further deposit spending the first
      -- assoc. list element.
      -- If hashedUserPkh > key, we filter elements and find the last suitable
      -- one.
      -- FIXME:
      -- LOGIC IDENTICAL TO StateDatum { maybeEntryName: Nothing } SO MERGE.
      case compare hashedUserPkh key of
        LT -> do
          logInfo' "userStakeBondedPoolContract: Compare LT"
          unless (minStake <= amt && amt <= maxStake)
            $ throwContractError
            "userStakeBondedPoolContract: Stake amount outside of min/max range"
           -- Minting a new Entry
          let
            mh = MintHead poolTxInput
            -- Minting a new Entry
            valRedeemer = Redeemer $ toData $ StakeAct
              { stakeAmount: amt
              , stakeHolder: userPkh
              , mintingAction: Just mh
              }
            mintRedeemer = Redeemer $ toData $ ListInsert mh
            -- Updated bonded state datum
            bondedStateDatum = Datum $ toData $ StateDatum
              { maybeEntryName: Just hashedUserPkh
              }
            amtBigInt = toBigInt amt
            -- The new Entry
            entryDatum = Datum $ toData $ Entry
              { key: hashedUserPkh
              , newDeposit: amtBigInt
              , deposited: amtBigInt
              , staked: zero
              , rewards: zero
              , next: Nothing
              }
            assetDatum = Datum $ toData AssetDatum
            stateTokenValue = singleton nftCs tokenName one
            assetParams = unwrap bondedAssetClass
            assetCs = assetParams.currencySymbol
            assetTn = assetParams.tokenName
            stakeValue = singleton assetCs assetTn amtBigInt
            entryValue = singleton assocListCs assocListTn one

          -- Get the minting policy and currency symbol from the list NFT:
          listPolicy <- liftedE $ mkListNFTPolicy Bonded nftCs

          bondedStateDatumLookup <-
            liftContractM
              "userStakeBondedPoolContract: Could not create state datum lookup"
              $ ScriptLookups.datum bondedStateDatum
          let
            constraints :: TxConstraints Unit Unit
            constraints =
              mconcat
                [ mustMintValueWithRedeemer mintRedeemer entryValue
                , mustPayToScript valHash bondedStateDatum stateTokenValue
                , mustPayToScript valHash assetDatum stakeValue
                , mustPayToScript valHash entryDatum entryValue
                , mustBeSignedBy userPkh
                , mustSpendScriptOutput poolTxInput valRedeemer
                ]

            lookup :: ScriptLookups.ScriptLookups PlutusData
            lookup = mconcat
              [ ScriptLookups.mintingPolicy listPolicy
              , ScriptLookups.validator validator
              , ScriptLookups.unspentOutputs $ unwrap userUtxos
              , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
              , bondedStateDatumLookup
              ]
          pure $ constraints /\ lookup
        EQ -> do
          -- POTENTIAL FIXME: Is this included on on chain code?
          -- If we have equality, the on chain element must already exist, so
          -- we must spend and update it.
          logInfo' "userStakeBondedPoolContract: Compare EQ"
          assocElem <- liftContractM "userStakeBondedPoolContract: Cannot extract\
            \head from Assoc. List - this should be impossible" $ head assocList
          let
            txIn /\ txOut = snd assocElem
            valRedeemer = Redeemer $ toData $ StakeAct
              { stakeAmount: amt
              , stakeHolder: userPkh
              , mintingAction: Nothing
              }
          -- Get the Entry datum of the old assoc. list element
          dHash <-  liftContractM
            "userStakeBondedPoolContract: Could not get Entry Datum Hash"
            (unwrap txOut).dataHash
          logInfo_ "userStakeBondedPoolContract: " dHash
          listDatum <- liftedM "userStakeBondedPoolContract: Cannot get Entry's\
            \ datum" $ getDatumByHash dHash
          bondedListDatum :: BondedStakingDatum <- liftContractM
            "userStakeBondedPoolContract: Cannot extract NFT State datum"
            $ fromData (unwrap listDatum)
          let amtBigInt = toBigInt amt
          -- The updated Entry Datum
          entryDatum <- case bondedListDatum of
            EntryDatum { entry } -> do
              let
                e = unwrap entry
                updateDeposited = e.deposited + amtBigInt
              unless (toBigInt minStake <= updateDeposited && updateDeposited <= toBigInt maxStake)
                $ throwContractError
                "userStakeBondedPoolContract: Stake amount outside of min/max \
                \range"
              pure $  Datum $ toData $ EntryDatum { entry: Entry $ e
                { newDeposit = e.newDeposit + amtBigInt
                , deposited = updateDeposited
                } }
            _ -> throwContractError"userStakeBondedPoolContract: Datum not \
              \Entry constructor"
          let
            assetDatum = Datum $ toData AssetDatum
            stateTokenValue = singleton nftCs tokenName one
            assetParams = unwrap bondedAssetClass
            assetCs = assetParams.currencySymbol
            assetTn = assetParams.tokenName
            stakeValue = singleton assetCs assetTn amtBigInt
            entryValue = singleton assocListCs assocListTn one

          -- Get the minting policy and currency symbol from the list NFT:
          listPolicy <- liftedE $ mkListNFTPolicy Bonded nftCs

          entryDatumLookup <-
            liftContractM
              "userStakeBondedPoolContract: Could not create state datum lookup"
              $ ScriptLookups.datum entryDatum
          let
            constraints :: TxConstraints Unit Unit
            constraints =
              mconcat
                [ mustPayToScript valHash assetDatum stakeValue
                , mustPayToScript valHash entryDatum entryValue
                , mustBeSignedBy userPkh
                , mustSpendScriptOutput txIn valRedeemer
                ]

            lookup :: ScriptLookups.ScriptLookups PlutusData
            lookup = mconcat
              [ ScriptLookups.validator validator
              , ScriptLookups.unspentOutputs $ unwrap userUtxos
              , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
              , entryDatumLookup
              ]
          pure $ constraints /\ lookup
        GT -> do
          -- The hashed key is greater than so we must look at the assoc. list
          -- in more detail
          logInfo' "userStakeBondedPoolContract: Compare GT"
          mintingAction /\ { firstInput, secondInput } /\ { firstOutput, secondOutput } /\ { firstKey, secondKey } <- liftContractM
            "userStakeBondedPoolContract: Cannot get position in Assoc. List"
            $ findAssocElem assocList hashedUserPkh
          let
            valRedeemer = Redeemer $ toData $ StakeAct
              { stakeAmount: amt
              , stakeHolder: userPkh
              , mintingAction
              }
            amtBigInt = toBigInt amt

          -- Get the Entry datum of the old assoc. list (first) element
          dHash <-  liftContractM
            "userStakeBondedPoolContract: Could not get Entry Datum Hash"
            (unwrap firstOutput).dataHash
          logInfo_ "userStakeBondedPoolContract: " dHash
          firstListDatum <- liftedM "userStakeBondedPoolContract: Cannot get Entry's\
            \ datum" $ getDatumByHash dHash
          firstBondedListDatum :: BondedStakingDatum <- liftContractM
            "userStakeBondedPoolContract: Cannot extract NFT State datum"
            $ fromData (unwrap firstListDatum)

          -- The updated first Entry Datum
          firstConstraints /\ firstLookups <- case firstBondedListDatum of
            EntryDatum { entry } -> do
              let e = unwrap entry
              firstEntryDatum <- case mintingAction of
                -- MintInBetween and MintEnd are the same here
                Just _ ->
                  -- a new middle entry is created so update next
                  pure $ Datum $ toData $ EntryDatum { entry: Entry $ e
                    { next = Just hashedUserPkh
                    } }
                Nothing -> do -- depositing/updating at the first entry
                  let updateDeposited = e.deposited + amtBigInt
                  unless (toBigInt minStake <= updateDeposited && updateDeposited <= toBigInt maxStake)
                    $ throwContractError
                    "userStakeBondedPoolContract: Stake amount outside of min/max \
                    \range"
                  pure $  Datum $ toData $ EntryDatum { entry: Entry $ e
                    { newDeposit = e.newDeposit + amtBigInt
                    , deposited = updateDeposited
                    } }
                _ -> throwContractError "userStakeBondedPoolContract: Shouldn't\
                  \be minting head"
              firstEntryDatumLookup <-
                liftContractM
                  "userStakeBondedPoolContract: Could not create state datum lookup"
                  $ ScriptLookups.datum firstEntryDatum
              listPolicy <- liftedE $ mkListNFTPolicy Bonded nftCs
              let
                valRedeemer = Redeemer $ toData $ StakeAct
                  { stakeAmount: amt
                  , stakeHolder: userPkh
                  , mintingAction
                  }
                entryValue = singleton assocListCs assocListTn one
                constr = mconcat
                  [ mustPayToScript valHash firstEntryDatum entryValue
                  , mustSpendScriptOutput firstInput valRedeemer
                  ]
                lu = mconcat
                  [ firstEntryDatumLookup
                  , ScriptLookups.mintingPolicy listPolicy
                  ]
              pure $ constr /\ lu
            _ -> throwContractError"userStakeBondedPoolContract: Datum not \
              \Entry constructor"
          middleConstraints /\ middleLookups <- if isJust mintingAction
            then do -- a genuine new entry
              unless (minStake <= amt && amt <= maxStake)
                $ throwContractError
                "userStakeBondedPoolContract: Stake amount outside of min/max range"
              -- Inbetween mint (FIXME - GET RID OF ISJUST)
              m <- liftContractM "userStakeBondedPoolContract: Could not get \
                \minting action" mintingAction
              let
                -- Minting a new Entry
                valRedeemer = Redeemer $ toData $ StakeAct
                  { stakeAmount: amt
                  , stakeHolder: userPkh
                  , mintingAction
                  }
                mintRedeemer = Redeemer $ toData $ ListInsert m

                entryDatum = Datum $ toData $ Entry
                  { key: hashedUserPkh
                  , newDeposit: amtBigInt
                  , deposited: amtBigInt
                  , staked: zero
                  , rewards: zero
                  , next: secondKey -- points to original second key
                  }
                assetDatum = Datum $ toData AssetDatum
                stateTokenValue = singleton nftCs tokenName one
                assetParams = unwrap bondedAssetClass
                assetCs = assetParams.currencySymbol
                assetTn = assetParams.tokenName
                stakeValue = singleton assetCs assetTn amtBigInt
                entryValue = singleton assocListCs assocListTn one

              -- Get the minting policy and currency symbol from the list NFT:
              listPolicy <- liftedE $ mkListNFTPolicy Bonded nftCs

              entryDatumLookup <-
                liftContractM
                  "userStakeBondedPoolContract: Could not create state datum lookup"
                  $ ScriptLookups.datum entryDatum
              let
                constr = mconcat
                  [ mustMintValueWithRedeemer mintRedeemer entryValue
                  , mustPayToScript valHash entryDatum entryValue
                  ]
                lu = mconcat
                  [ entryDatumLookup
                  , ScriptLookups.mintingPolicy listPolicy
                  ]
              pure $ constr /\ lu
            else pure $ mempty /\ mempty

           -- Get the Entry datum of the old assoc. list (second) element
          lastConstraints /\ lastLookups <- case secondOutput, secondInput of
            Nothing, Nothing -> pure $ mempty /\ mempty
            Just so, Just si -> do --
              dHash <-  liftContractM
                "userStakeBondedPoolContract: Could not get Entry Datum Hash"
                (unwrap so).dataHash
              logInfo_ "userStakeBondedPoolContract: " dHash
              secondListDatum <- liftedM "userStakeBondedPoolContract: Cannot get Entry's\
                \ datum" $ getDatumByHash dHash
              secondBondedListDatum :: BondedStakingDatum <- liftContractM
                "userStakeBondedPoolContract: Cannot extract NFT State datum"
                $ fromData (unwrap secondListDatum)

              -- Unchanged in the case
              lastEntryDatum <- case secondBondedListDatum of
                EntryDatum { entry } ->
                  pure $ Datum $ toData $ EntryDatum { entry }
                _ -> throwContractError"userStakeBondedPoolContract: Datum not \
                  \Entry constructor"

              let
                valRedeemer = Redeemer $ toData $ StakeAct
                  { stakeAmount: amt
                  , stakeHolder: userPkh
                  , mintingAction
                  }
              -- Get the minting policy and currency symbol from the list NFT:
              listPolicy <- liftedE $ mkListNFTPolicy Bonded nftCs

              lastEntryDatumLookup <-
                liftContractM
                  "userStakeBondedPoolContract: Could not create state datum lookup"
                  $ ScriptLookups.datum lastEntryDatum
              let
                entryValue = singleton assocListCs assocListTn one
                constr = mconcat
                  [ mustPayToScript valHash lastEntryDatum entryValue
                  , mustSpendScriptOutput si valRedeemer
                  ]
                lu = mconcat
                  [ ScriptLookups.mintingPolicy listPolicy
                  , lastEntryDatumLookup
                  ]
              pure $ constr /\ lu
            _, _ -> throwContractError"userStakeBondedPoolContract: Datum not\
              \Entry constructor"
          pure $
            mconcat
              [ firstConstraints
              , middleConstraints
              , lastConstraints
              , mustBeSignedBy userPkh
              ]
            /\
              mconcat
                [ ScriptLookups.validator validator
                , ScriptLookups.unspentOutputs $ unwrap userUtxos
                , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
                , firstLookups
                , middleLookups
                , lastLookups
                ]
    _ -> throwContractError "findAssocLuserStakeBondedPoolContractistUtxo: \
          \Datum incorrect type"

  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  logInfo_
    "userStakeBondedPoolContract: unAttachedUnbalancedTx"
    unattachedBalancedTx
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "userStakeBondedPoolContract: Cannot balance, reindex redeemers, attach \
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "userStakeBondedPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash
