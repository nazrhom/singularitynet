{-# LANGUAGE RecordWildCards #-}
module Utils (
    testAdminWallet
    , testUserWallet
    , testUserStake
    , testAdminDeposit
    , testInitialBondedParams
    , createBondedPool
    , userHeadStake
    , poolDeposit
    , bondedPoolInitialDatum
    , getOwnData
    , getFirstUtxo
    , mkBondedPoolParams
    , currentRoundedTime
    , logInfo
    , logInfo'
) where
import Test.Plutip.Contract ( initAda, TestWallets )
import Plutus.Contract qualified as Contract
import Plutus.Contract (Contract, EmptySchema)
import Cardano.Prelude (Text, listToMaybe)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), ChainIndexTxOut, PubKeyHash (PubKeyHash), Datum (Datum), TxOut)
import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Plutus.V1.Ledger.Api (Address, BuiltinByteString, TxOutRef, Script, Value, TxOut (TxOut), BuiltinData, FromData, toData, TokenName (TokenName))
import Data.Map (Map)
import Data.Map qualified as Map
import PlutusTx.Builtins (blake2b_256)
import Plutus.V1.Ledger.Api (CurrencySymbol)
import Types(InitialBondedPoolParams(..), BondedPoolScripts(..), BondedPoolTypedScripts (..), TBondedPool (TBondedPool, tbpTxOutRef, tbpStateDatum), TEntry (TEntry, teEntryDatum, teTxOutRef))
import SingularityNet.Types (BondedPoolParams(..), BondedStakingDatum (StateDatum, EntryDatum, AssetDatum), AssetClass (AssetClass), Entry (..), BondedStakingAction (StakeAct, AdminAct), ListAction (ListInsert), MintingAction (MintHead))
import Ledger.Contexts (scriptCurrencySymbol)
import Plutus.V1.Ledger.Api (singleton, ToData (toBuiltinData), adaSymbol, adaToken)
import SingularityNet.Settings (bondedStakingTokenName)
import Ledger.Constraints (ScriptLookups, TxConstraints)
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Tx qualified as Tx
import PlutusTx qualified
import Data.Void (Void)
import Ledger.Scripts (applyArguments)
import Data.Ratio ((%))
import SingularityNet.Natural (NatRatio(NatRatio), Natural (Natural), toPOSIXTime)
import Plutus.V1.Ledger.Value (valueOf)
import Control.Monad (unless, when)
import Data.Text (pack)
import Plutus.V1.Ledger.Api (POSIXTimeRange)
import Plutus.V1.Ledger.Api (POSIXTime)
import Plutus.V1.Ledger.Interval (interval)
import Control.Monad (void)
import Ledger.TimeSlot (slotToPOSIXTimeRange)

-- The first UTxO is used for initializing the pool
testAdminWallet :: TestWallets
testAdminWallet = initAda [5, 10_000, 10_000]

-- Another UTxO just in case
testUserWallet :: TestWallets
testUserWallet = initAda [10_000, 10_000]

testUserStake :: Natural
testUserStake = Natural 2000

testAdminDeposit :: Natural
testAdminDeposit = Natural 10

-- We use Ada as bonded asset because we can't initialize wallets with tokens
-- we are not able to mint
testInitialBondedParams :: InitialBondedPoolParams
testInitialBondedParams = InitialBondedPoolParams {
   initIterations = Natural 3
  , initStart = 1000
  , initEnd = 2000
  , initUserLength = 100
  , initBondingLength = 4
  , initInterest = NatRatio $ 1 % 100
  , initMinStake = Natural 1000
  , initMaxStake = Natural 10_000
    -- We use Ada for now because it's not straightforward to use AGIX or NTX
    -- in plutip (can only use mintable tokens)
  , initBondedAssetClass = AssetClass adaSymbol adaToken 
}

-- The first datum a bonded pool should have
bondedPoolInitialDatum :: BondedStakingDatum
bondedPoolInitialDatum = StateDatum Nothing

-- Get the admin's data
getOwnData ::
    Contract
        String
        EmptySchema
        Text
        ( PaymentPubKeyHash             -- Own pub key hash
        , BuiltinByteString             -- Own key (for entry token)
        , Map TxOutRef ChainIndexTxOut  -- Own UTxOs
        )
getOwnData = do
  ownPkh@(PaymentPubKeyHash (PubKeyHash bs)) <- Contract.ownPaymentPubKeyHash
  let ownAddress :: Address
      ownAddress = Ledger.pubKeyHashAddress ownPkh Nothing
      ownKey :: BuiltinByteString
      ownKey = blake2b_256 bs
  ownUtxos <- Contract.utxosAt ownAddress
  pure (ownPkh, ownKey, ownUtxos)
  
getUserData ::
    PaymentPubKeyHash ->
    Contract
        String
        EmptySchema
        Text
        ( PaymentPubKeyHash             -- Own pub key hash
        , BuiltinByteString             -- Own key (for entry token)
        , Map TxOutRef ChainIndexTxOut  -- Own UTxOs
        )
getUserData userPkh = do
  let (PaymentPubKeyHash (PubKeyHash bs)) = userPkh
      userAddress :: Address
      userAddress = Ledger.pubKeyHashAddress userPkh Nothing
      userKey :: BuiltinByteString
      userKey = blake2b_256 bs
  userUtxos <- Contract.utxosAt userAddress
  pure (userPkh, userKey, userUtxos)
    
  
-- Get first UTxO at a given address
getFirstUtxo :: Address -> Contract String EmptySchema Text (Maybe (TxOutRef, ChainIndexTxOut))
getFirstUtxo addr = listToMaybe . Map.toList <$> Contract.utxosAt addr

-- Gets the UTxOs that satisfy the predicate on its Value
findUtxosWithValue :: (Value -> Bool) -> Map TxOutRef TxOut -> Map TxOutRef TxOut 
findUtxosWithValue pred = Map.filter
    (\TxOut { txOutValue } -> pred txOutValue)
    
-- Gets the UTxOs with a datum hash that match the given value
findUtxosWithDatum :: forall (a :: Type) . ToData a => a -> Map TxOutRef TxOut -> Map TxOutRef TxOut
findUtxosWithDatum x = Map.filter $
    \TxOut { txOutDatumHash } -> case txOutDatumHash of
        Just dh -> dh == hash x
        Nothing -> False
    where hash = Ledger.datumHash . Datum . toBuiltinData

-- Gets the new UTxO of the pool from a succesful transaction
findPoolUtxo :: BondedPoolParams -> Map TxOutRef TxOut -> Maybe TxOutRef
findPoolUtxo bpp = (fst <$>) . listToMaybe . Map.toList . findUtxosWithValue
    (\val -> valueOf val (nftCs bpp) bondedStakingTokenName == 1)

-- Gets the UTxO of an entry from a succesful transaction
findEntryUtxo :: BondedPoolParams -> BuiltinByteString -> Map TxOutRef TxOut -> Maybe TxOutRef
findEntryUtxo bpp key =
    (fst <$>) . listToMaybe . Map.toList . findUtxosWithValue
        (\val -> valueOf val (assocListCs bpp) (TokenName key) == 1)


-- Create a pool from an initial UTxO and the inital pool parameters. It returns
-- the full parameters of the pool, the applied scripts and requiried pool
-- UTxO information
createBondedPool ::
    BondedPoolScripts ->
    InitialBondedPoolParams ->
    Contract String EmptySchema Text (BondedPoolTypedScripts, BondedPoolParams, TBondedPool)
createBondedPool scripts initialParams = do
  -- Get admin's data
  adminData@(pkh, _, ownUtxos) <- getOwnData
  logInfo "(createBondedPool) Admin's data" adminData
  -- Get first utxo (used to init the pool)
  let initUtxo = fst . head $ Map.toList ownUtxos
  logInfo "(createBondedPool) initUtxo:" initUtxo
  -- Get the pool's scripts and parameters
  let (bps@BondedPoolTypedScripts {..}, bpp) =
        initPool scripts pkh initUtxo initialParams
  -- Calculate CS, Value and initial state of bonded pool
  let stateCs = scriptCurrencySymbol statePolicy
      stateNft = singleton stateCs bondedStakingTokenName 1
      stateDatum = StateDatum Nothing
      datum = Datum . toBuiltinData  $ stateDatum
      -- Prepare lookups and constraints
      valHash = Ledger.validatorHash validator
      lookups :: ScriptLookups Void
      lookups =
        Constraints.mintingPolicy statePolicy <>
        Constraints.otherScript validator <>
        Constraints.unspentOutputs ownUtxos
      constraints :: TxConstraints Void Void
      constraints =
        Constraints.mustPayToOtherScript valHash datum stateNft
          <> Constraints.mustMintValue stateNft
          <> Constraints.mustSpendPubKeyOutput initUtxo
  -- Submit and await transaction
  tx <- Contract.submitTxConstraintsWith lookups constraints
  Contract.awaitTxConfirmed $ Tx.getCardanoTxId tx
  -- Get new `TxOutRef` of the pool
  let txOutputs = Ledger.getCardanoTxUnspentOutputsTx tx
  poolUtxo <- liftContractM "createBondedPool: Could not get new UTxO of the pool" $
    findPoolUtxo bpp txOutputs
  pure (bps, bpp, TBondedPool { tbpTxOutRef = poolUtxo , tbpStateDatum = stateDatum })
  
userHeadStake ::
    [PaymentPubKeyHash] ->
    BondedPoolTypedScripts ->
    BondedPoolParams ->
    TBondedPool -> -- Current pool state
    Natural -> -- Stake amount
    Contract String EmptySchema Text (TBondedPool, TEntry) -- Inserted/Updated entry and state
userHeadStake
    [userPPkh]
    BondedPoolTypedScripts {..}
    bpp@BondedPoolParams {..}
    TBondedPool {..}
    stakeAmt@(Natural stakeAmtNat) = do
        (PaymentPubKeyHash userPkh, userKey, userUtxos) <- getUserData userPPkh
        -- Calculate time range of the transaction and wait the appropriate time if necessary
        currTime <- Contract.currentTime
        info@(_currApproxTime, rangeStart, rangeEnd) <- getStakingTime bpp
        let timeRange :: POSIXTimeRange
            timeRange = interval rangeStart rangeEnd
        -- Contract.throwError $ pack $ "timeRange: " <> show timeRange <> " " <> show info
        when (currTime < rangeStart) $
            void $ Contract.awaitTime rangeStart
        when (currTime > rangeEnd) $
            void $ Contract.throwError "userHeadStake: timeRange missed"
        -- Get pool utxos
        let valHash = Ledger.validatorHash validator
            poolAddr = Ledger.scriptAddress validator
        poolUtxos <- Contract.utxosAt poolAddr
        case tbpStateDatum of
          -- This is a stake update
          StateDatum (Just _headKey) ->
            Contract.throwError "userHeadStake: updates not implemented"
          -- This is the first stake of the pool
          StateDatum Nothing -> do
            -- Check bounds of stake
            unless (minStake <= stakeAmt && stakeAmt <= maxStake) $
                Contract.throwError "userHeadStake: stake amount too \
                \large/small"
            let
                -- Prepare new entry
                entry = Entry {
                    key = userKey
                    , newDeposit = stakeAmt
                    , deposited = stakeAmt
                    , staked = Natural 0
                    , rewards = NatRatio 0
                    , next = Nothing
                }
                entryDatum = Ledger.Datum . toBuiltinData $ EntryDatum entry
                -- Prepare new state
                newState = StateDatum $ Just userKey
                stateDatum = Ledger.Datum . toBuiltinData $ newState
                -- Prepare asset datum
                assetDatum = Ledger.Datum . toBuiltinData $ AssetDatum
                -- Prepare values
                entryVal = singleton assocListCs (TokenName userKey) 1
                stateVal = singleton nftCs bondedStakingTokenName 1
                stakeVal = singleton adaSymbol adaToken (fromIntegral stakeAmtNat)
                -- Prepare minting and list action
                mintAct = MintHead tbpTxOutRef
                listAct = ListInsert mintAct
                -- Prepare policy redeemer
                mintRedeemer = Ledger.Redeemer . toBuiltinData $ listAct
                -- Prepare validator redeemer
                valRedeemer = Ledger.Redeemer $ toBuiltinData $ StakeAct
                    stakeAmt
                    userPkh
                    (Just mintAct)
                -- Prepare lookups and constraints
                lookups :: ScriptLookups Void
                lookups =
                  Constraints.mintingPolicy listPolicy
                  <> Constraints.otherScript validator
                  <> Constraints.otherData stateDatum
                  <> Constraints.otherData entryDatum
                  <> Constraints.otherData assetDatum
                  <> Constraints.unspentOutputs userUtxos
                  <> Constraints.unspentOutputs poolUtxos
                constraints :: TxConstraints Void Void
                constraints =
                  Constraints.mustMintValueWithRedeemer mintRedeemer entryVal
                    <> Constraints.mustPayToOtherScript valHash stateDatum stateVal
                    <> Constraints.mustPayToOtherScript valHash entryDatum entryVal
                    <> Constraints.mustPayToOtherScript valHash assetDatum stakeVal
                    <> Constraints.mustBeSignedBy userPPkh
                    <> Constraints.mustSpendScriptOutput tbpTxOutRef valRedeemer
                    <> Constraints.mustValidateIn timeRange

            -- Submit and await transaction
            tx <- Contract.submitTxConstraintsWith lookups constraints
            Contract.awaitTxConfirmed $ Tx.getCardanoTxId tx
            -- Get outrefs of the pool and the new entry
            let txOutputs = Ledger.getCardanoTxUnspentOutputsTx tx
            newPoolUtxo <- liftContractM "userHeadStake: Could not get new UTxO\
                \ of the pool" $
                findPoolUtxo bpp txOutputs
            entryUtxo <- liftContractM "userHeadStake: Could not get new UTxO\
                \ of the entry" $
                findEntryUtxo bpp userKey txOutputs
            pure (TBondedPool { tbpTxOutRef = newPoolUtxo , tbpStateDatum = newState },
             TEntry { teTxOutRef = entryUtxo, teEntryDatum = EntryDatum entry})
            
          EntryDatum _ -> Contract.throwError "userHeadStake: received an Entry"
          AssetDatum -> Contract.throwError "userHeadStake: received an AssetDatum"
userHeadStake _ _ _ _ _ = Contract.throwError "userHeadStake: not called with one wallet"

-- This is a stub, it consumes the correct outputs but it does not update any datum
-- or deposits the correct amount
poolDeposit ::
    BondedPoolTypedScripts ->
    BondedPoolParams ->
    TBondedPool ->                                           -- Current pool state
    [TEntry] ->                                              -- Entries to update
    Natural ->                                               -- Deposit amount
    Contract String EmptySchema Text (TBondedPool, [TEntry])
poolDeposit
    BondedPoolTypedScripts {..}
    bpp@BondedPoolParams {..}
    TBondedPool {..}
    entries
    (Natural depositAmtNat) = do
        (ownPPkh, _, ownUtxos) <- getOwnData
        -- Get pool utxos
        let valHash = Ledger.validatorHash validator
            poolAddr = Ledger.scriptAddress validator
        poolUtxos <- Contract.utxosAt poolAddr
        -- Get entry
        let TEntry{..} = head entries
        entry <- liftContractM "poolDeposit: could not get entry from TEntry" $
            case teEntryDatum of
                EntryDatum e -> Just e
                _ -> Nothing
        let
            -- Repeat entry
            entryDatum = Ledger.Datum . toBuiltinData . EntryDatum $ entry
            userKey = key entry
            -- Repeat state
            stateDatum = Datum $ toBuiltinData tbpStateDatum
            -- Prepare asset datum
            assetDatum = Ledger.Datum . toBuiltinData $ AssetDatum
            -- Prepare values
            entryVal = singleton assocListCs (TokenName userKey) 1
            stateVal = singleton nftCs bondedStakingTokenName 1
            depositVal = singleton adaSymbol adaToken (fromIntegral depositAmtNat)
            -- Prepare validator redeemer
            valRedeemer = Ledger.Redeemer . toBuiltinData $ AdminAct
            -- Prepare lookups and constraints
            lookups :: ScriptLookups Void
            lookups =
              Constraints.otherScript validator
              <> Constraints.otherData stateDatum
              <> Constraints.otherData entryDatum
              <> Constraints.otherData assetDatum
              <> Constraints.unspentOutputs ownUtxos
              <> Constraints.unspentOutputs poolUtxos
            constraints :: TxConstraints Void Void
            constraints =
                Constraints.mustPayToOtherScript valHash stateDatum stateVal
                <> Constraints.mustPayToOtherScript valHash entryDatum entryVal
                <> Constraints.mustPayToOtherScript valHash assetDatum depositVal
                <> Constraints.mustBeSignedBy ownPPkh
                <> Constraints.mustSpendScriptOutput tbpTxOutRef valRedeemer
                <> Constraints.mustSpendScriptOutput teTxOutRef valRedeemer

            -- Submit and await transaction
        tx <- Contract.submitTxConstraintsWith lookups constraints
        Contract.awaitTxConfirmed $ Tx.getCardanoTxId tx
        -- Get outrefs of the pool and the new entry
        let txOutputs = Ledger.getCardanoTxUnspentOutputsTx tx
        newPoolUtxo <- liftContractM "poolDeposit: Could not get new UTxO\
            \ of the pool" $
            findPoolUtxo bpp txOutputs
        entryUtxo <- liftContractM "poolDeposit: Could not get new UTxO\
            \ of the entry" $
            findEntryUtxo bpp userKey txOutputs
        pure (TBondedPool { tbpTxOutRef = newPoolUtxo , tbpStateDatum},
              [TEntry { teTxOutRef = entryUtxo, teEntryDatum = EntryDatum entry}])
-- Create the parameters of the pool from the initial params and other
-- information obtained at runtime
mkBondedPoolParams ::
    PaymentPubKeyHash ->
    CurrencySymbol ->
    CurrencySymbol ->
    InitialBondedPoolParams ->
    BondedPoolParams
mkBondedPoolParams (PaymentPubKeyHash pkh) stateCs listCs InitialBondedPoolParams {..} =
    BondedPoolParams {
        iterations = initIterations,
        start = initStart,
        end = initEnd,
        userLength = initUserLength,
        bondingLength = initBondingLength,
        interest = initInterest,
        minStake = initMinStake,
        maxStake = initMaxStake,
        admin = pkh,
        bondedAssetClass = initBondedAssetClass,
        nftCs = stateCs,
        assocListCs = listCs
    }
    
-- Create the validator and the policies from the scripts, the initial
-- `TxOutRef`, an operator's UTxO and the initial parameters of the pool
initPool ::
    BondedPoolScripts ->
    PaymentPubKeyHash ->
    TxOutRef ->
    InitialBondedPoolParams ->
    (BondedPoolTypedScripts, BondedPoolParams)
initPool
    BondedPoolScripts{statePolicyScript, listPolicyScript, validatorScript}
    adminPkh
    outRef
    initialParams =
        -- These definitions are mutually recursive but the execution terminates
        -- (notice that `statePolicy` does not depend on `bondedPoolParams`)
        let bondedPoolScripts :: BondedPoolTypedScripts
            bondedPoolScripts = BondedPoolTypedScripts {
                validator = Ledger.Validator $ applyArguments
                    validatorScript
                    [PlutusTx.toData bondedPoolParams]
                , statePolicy = Ledger.MintingPolicy $ applyArguments
                    statePolicyScript
                    [PlutusTx.toData outRef] 
                , listPolicy = Ledger.MintingPolicy $ applyArguments
                        listPolicyScript
                        [PlutusTx.toData $ nftCs bondedPoolParams]
        }
            bondedPoolParams :: BondedPoolParams
            bondedPoolParams = mkBondedPoolParams
                adminPkh
                (scriptCurrencySymbol $ statePolicy bondedPoolScripts)
                (scriptCurrencySymbol $ listPolicy bondedPoolScripts)
                initialParams
        in (bondedPoolScripts, bondedPoolParams)

getStakingTime :: BondedPoolParams -> Contract String EmptySchema Text (POSIXTime, POSIXTime, POSIXTime)
getStakingTime bpp@BondedPoolParams{..} = do
    -- Get time and round it up to the nearest second
    currTime <- currentRoundedTime
    -- Throw error if staking is impossible
    when (currTime > end) $
        Contract.throwError "getStakingTime: pool already closed"
    when (currTime > end - userLength) $
        Contract.throwError "getStakingTime: pool in withdrawing only period"
    -- Get timerange in which the staking should be performed
    let cycleLength :: POSIXTime
        cycleLength = bondingLength + userLength
        possibleRanges :: [(POSIXTime, POSIXTime)]
        possibleRanges = filter (\r -> snd r > currTime) $
            [(start + n * cycleLength, start + n * cycleLength + userLength - 1000)
                | n <- [0 .. toPOSIXTime iterations]]
    case possibleRanges of
        (s, e) : _ -> pure (currTime, s, e)
        _ -> Contract.throwError "getStakingTime: no more staking periods available"

getBondingTime :: BondedPoolParams -> Contract String EmptySchema Text (POSIXTime, POSIXTime, POSIXTime)
getBondingTime bpp@BondedPoolParams{..} = do
    currTime <- currentRoundedTime
    -- Throw error if bonding is impossible
    when (currTime > end) $
        Contract.throwError "getBondingTime: pool already closed"
    when (currTime > end - userLength) $
        Contract.throwError "getBondingTime: pool in withdrawing only period"
    -- Get timerange in which the staking should be performed
    let cycleLength :: POSIXTime
        cycleLength = bondingLength + userLength
        possibleRanges :: [(POSIXTime, POSIXTime)]
        possibleRanges = filter (\r -> snd r > currTime) $
            [(start + n * cycleLength + userLength, start + (n+1) * cycleLength - 1000)
                | n <- [0 .. toPOSIXTime iterations]]
    case possibleRanges of
        (s, e) : _ -> pure (currTime, s, e)
        _ -> Contract.throwError "getBondingTime: no more bonding periods available"

-- Returns the ceiling of the current time in seconds
currentRoundedTime :: Contract String EmptySchema Text POSIXTime
currentRoundedTime = do
    currTime <- Contract.currentTime
    pure $ ceiling (fromIntegral @POSIXTime @Double currTime / 1000) * 1000

logInfo :: Show a => String -> a -> Contract w s e ()
logInfo msg a = Contract.logInfo (msg <> ": " <> show a)

logInfo' :: String -> Contract w s e ()
logInfo' = Contract.logInfo @String

liftContractM :: e -> Maybe a -> Contract w s e a
liftContractM _ (Just a) = pure a
liftContractM msg Nothing = Contract.throwError msg
