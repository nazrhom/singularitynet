module CreatePool (createPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , validatorHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftContractM, liftedE, liftedE', liftedM)
import Contract.PlutusData (PlutusData, Datum(Datum), toData)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
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
import Settings (bondedStakingTokenName, hardCodedParams)
import Scripts.BondedListNFT (mkBondedListNFTPolicy)
import Scripts.BondedPoolValidator (mkBondedPoolValidator)
import Scripts.BondedStateNFT (mkBondedStateNFTPolicy)
import Types (BondedStakingDatum(StateDatum), PoolInfo)
import Utils (logInfo_, nat)

-- Sets up pool configuration, mints the state NFT and deposits
-- in the pool validator's address
createPoolContract :: Contract () PoolInfo
createPoolContract = do
  networkId <- getNetworkId
  adminPkh <- liftedM "createPoolContract: Cannot get admin's pkh"
    ownPaymentPubKeyHash
  logInfo_ "Admin PaymentPubKeyHash" adminPkh
  -- Get the (Nami) wallet address
  adminAddr <- liftedM "createPoolContract: Cannot get wallet Address"
    getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "createPoolContract: Cannot get user Utxos" $ utxosAt adminAddr
  txOutRef <- liftContractM "createPoolContract: Could not get head UTXO"
    $ fst
    <$> (head $ toUnfoldable $ unwrap adminUtxos)
  -- Get the minting policy and currency symbol from the state NFT:
  statePolicy <- liftedE $ mkBondedStateNFTPolicy txOutRef
  stateNftCs <-
    liftedM "createPoolContract: Cannot get CurrencySymbol from state NFT"
      $ scriptCurrencySymbol statePolicy
  -- Get the minting policy and currency symbol from the list NFT:
  listPolicy <- liftedE $ mkBondedListNFTPolicy stateNftCs
  assocListCs <-
    liftedM "createPoolContract: Cannot get CurrencySymbol from state NFT"
      $ scriptCurrencySymbol listPolicy
  -- May want to hardcode this somewhere:
  tokenName <- liftContractM "createPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  -- We define the parameters of the pool
  params <- liftContractM "createPoolContract: Failed to create parameters" $
    hardCodedParams adminPkh stateNftCs assocListCs
  -- Get the bonding validator and hash
  validator <- liftedE' "createPoolContract: Cannot create validator" $
    mkBondedPoolValidator params
  valHash <- liftedM "createPoolContract: Cannot hash validator"
    (validatorHash validator)
  let
    mintValue = singleton stateNftCs tokenName one
    poolAddr = validatorHashEnterpriseAddress networkId valHash
  logInfo_ "BondedPool Validator's address" poolAddr
  let
    bondedStateDatum = Datum $ toData $ StateDatum
      { maybeEntryName: Nothing
      , sizeLeft: nat 100
      }

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.mintingPolicy statePolicy
      , ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap adminUtxos
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [ mustPayToScript valHash bondedStateDatum mintValue
        , mustMintValue mintValue
        , mustSpendPubKeyOutput txOutRef
        ]

  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  -- `balanceAndSignTx` does the following:
  -- 1) Balance a transaction
  -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
  -- 3) Attach datums and redeemers to transaction.
  -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "createPoolContract: Cannot balance, reindex redeemers, attach datums/\
      \redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_ "createPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash
  -- Return the pool info for subsequent transactions
  pure $ wrap { stateNftCs, assocListCs, poolAddr }


--  balancedTx: (
--    Transaction {
--      auxiliaryData: Nothing,
--      body: (TxBody {
--        auxiliaryDataHash: Nothing,
--        certs: Nothing,
--        collateral: (Just [
--          (TransactionInput {
--            index: 0u,
--            transactionId: (
--              TransactionHash (byteArrayFromIntArrayUnsafe
--              [180,118,245,6,211,153,203,40,137,14,84,193,134,206,114,200,197,202,79,101,208,187,247,156,184,125,64,75,98,49,68,156])) })]),
--       fee: (Coin fromString "908137"),
--       inputs: [(
--         TransactionInput {
--           index: 1u,
--           transactionId: (TransactionHash (byteArrayFromIntArrayUnsafe
--             [75,174,129,233,42,202,194,25,59,124,75,19,119,131,95,42,102,145,114,50,190,176,225,32,60,104,170,153,190,230,83,163])) }),
--         (TransactionInput {
--           index: 1u,
--           transactionId: (TransactionHash (byteArrayFromIntArrayUnsafe
--             [75,174,129,233,42,202,194,25,59,124,75,19,119,131,95,42,102,145,114,50,190,176,225,32,60,104,170,153,190,230,83,163])) }),
--         (TransactionInput {
--           index: 0u,
--           transactionId: (TransactionHash (byteArrayFromIntArrayUnsafe
--             [176,62,129,229,99,92,2,251,34,236,14,217,71,239,59,160,190,68,197,128,41,232,218,154,8,244,229,57,229,199,54,236])) })],
--       mint: (Just (Mint (
--         NonAdaAsset(fromFoldable [(Tuple (
--           CurrencySymbol(byteArrayFromIntArrayUnsafe
--             [145,90,227,106,233,31,106,223,219,214,40,21,223,134,141,180,170,92,204,193,164,22,217,255,166,122,173,38])) (fromFoldable [(Tuple (
--           TokenName(byteArrayFromIntArrayUnsafe [66,111,110,100,101,100,83,116,97,107,105,110,103,84,111,107,101,110])) fromString "1")]))])))),
--       networkId: (Just TestnetId),
--       outputs: [(
--         TransactionOutput {
--           address: (Address addr_test1qpkl55y6av6lvu9gfjkqr0cmtkxn7qcezgg0q3vr0m2huda6qh8x3elu6qa9t48ymn0dluh805ws2we38uxwxcnzyygq5jl946),
--           amount: (Value (
--             Coin fromString "1000519431") (
--               NonAdaAsset(fromFoldable [(Tuple (
--                 CurrencySymbol(byteArrayFromIntArrayUnsafe
--                 [181,9,79,147,255,159,203,169,232,178,87,25,125,88,156,188,222,61,146,161,8,128,78,58,55,139,210,206]))
--                 (fromFoldable [(Tuple (TokenName(byteArrayFromIntArrayUnsafe [78,84,88])) fromString "2000000000000")]))]))),
--           dataHash: Nothing }),(
--         TransactionOutput {
--           address: (Address addr_test1wzmgdl90h32tlgzd0d6aguddwfzanv2f3htet9jnsq79p9g5yyzw6),
--           amount: (Value
--             (Coin fromString "1862028")
--               (NonAdaAsset(fromFoldable [(Tuple (
--                 CurrencySymbol(byteArrayFromIntArrayUnsafe
--                 [145,90,227,106,233,31,106,223,219,214,40,21,223,134,141,180,170,92,204,193,164,22,217,255,166,122,173,38]))
--                 (fromFoldable [(Tuple (
--                   TokenName(byteArrayFromIntArrayUnsafe
--                     [66,111,110,100,101,100,83,116,97,107,105,110,103,84,111,107,101,110]))
--                 fromString "1")]))]))),
--           dataHash: (Just (
--             DataHash (byteArrayFromIntArrayUnsafe
--               [29,177,145,147,35,214,143,18,8,50,84,63,196,185,11,84,25,12,110,191,246,92,218,165,223,182,50,82,146,21,224,75]))) })],
--           requiredSigners: Nothing,
--           scriptDataHash: Nothing,
--           ttl: Nothing,
--           update: Nothing,
--           validityStartInterval: Nothing,
--           withdrawals: Nothing }),
--           isValid: true,
--           witnessSet: (
--             TransactionWitnessSet {
--               bootstraps: Nothing,
--               nativeScripts: Nothing,
--               plutusData: Nothing,
--               plutusScripts: (Just [(PlutusScript
--                 (byteArrayFromIntArrayUnsafe
--                   [89,2,130,1,0,0,51,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,34,34,35,35,35,51,34,35,35,35,37,51,48,20,83,48,18,51,35,0,226,37,51,48,17,0,17,74,2,166,100,70,102,3,32,4,41,68,0,76,0,204,6,192,4,76,0,140,6,128,4,140,221,120,3,152,12,152,13,0,9,186,195,1,128,2,19,51,34,35,51,50,34,35,51,1,66,34,83,51,1,128,2,20,160,38,96,50,96,2,96,68,0,70,96,6,96,66,0,64,2,0,36,166,102,3,134,0,166,235,140,7,128,4,84,204,7,82,65,9,101,118,97,108,67,115,32,79,75,0,19,51,1,82,34,83,51,1,144,2,20,160,38,96,52,96,2,96,70,0,70,96,6,96,68,0,64,2,110,172,192,136,0,73,76,204,7,84,204,6,204,1,77,215,24,15,128,8,152,2,27,173,48,35,0,17,83,48,30,73,1,18,101,118,97,108,84,110,65,110,100,65,109,111,117,110,116,32,79,75,0,20,162,42,102,3,201,33,43,112,114,101,100,105,99,97,116,101,32,111,110,32,84,111,107,101,110,78,97,109,101,47,97,109,111,117,110,116,32,110,111,116,32,115,97,116,105,115,102,105,101,100,0,20,160,42,102,3,169,33,41,112,114,101,100,105,99,97,116,101,32,111,110,32,67,117,114,114,101,110,99,121,83,121,109,98,111,108,32,110,111,116,32,115,97,116,105,115,102,105,101,100,0,20,160,110,60,0,205,199,128,17,145,25,184,144,1,0,36,128,8,0,76,140,140,148,204,192,92,205,195,164,0,0,4,38,235,140,6,192,4,84,204,6,18,65,25,110,111,116,32,97,32,109,105,110,116,105,110,103,32,116,114,97,110,115,97,99,116,105,111,110,0,22,48,28,0,35,1,112,1,55,84,96,48,96,46,0,105,17,18,66,111,110,100,101,100,83,116,97,107,105,110,103,84,111,107,101,110,0,55,86,96,48,0,34,147,11,25,24,11,152,11,128,9,128,176,0,152,11,152,11,0,9,128,176,0,153,186,84,128,0,204,5,76,221,42,64,0,102,2,166,234,77,215,0,48,11,25,128,169,186,131,117,160,10,2,201,48,1,155,173,0,67,117,192,8,70,70,0,68,102,0,64,4,0,36,96,4,70,96,4,0,64,2,170,231,200,140,0,204,221,121,128,32,1,24,2,0,9,41,153,128,40,0,138,80,20,162,70,110,148,204,192,16,0,82,0,36,128,0,3,8,140,204,1,0,8,0,64,12,82,130,185,165,115,138,174,117,93,18,186,18,48,2,55,84,0,42,174,121,93,2,94,184,19,1,43,216,121,159,216,121,159,88,32,75,174,129,233,42,202,194,25,59,124,75,19,119,131,95,42,102,145,114,50,190,176,225,32,60,104,170,153,190,230,83,163,255,1,255,0,1]))]),
--               redeemers: Nothing,
--               vkeys: Nothing }) })

  -- adminUtxos: (fromFoldable [(Tuple (TransactionInput { index: 1u, transactionId: (TransactionHash (byteArrayFromIntArrayUnsafe [75,174,129,233,42,202,194,25,59,124,75,19,119,131,95,42,102,145,114,50,190,176,225,32,60,104,170,153,190,230,83,163])) }) (TransactionOutput { address: (Address addr_test1qpkl55y6av6lvu9gfjkqr0cmtkxn7qcezgg0q3vr0m2huda6qh8x3elu6qa9t48ymn0dluh805ws2we38uxwxcnzyygq5jl946), amount: (Value (Coin fromString "1644798") (NonAdaAsset(fromFoldable [(Tuple (CurrencySymbol(byteArrayFromIntArrayUnsafe [181,9,79,147,255,159,203,169,232,178,87,25,125,88,156,188,222,61,146,161,8,128,78,58,55,139,210,206])) (fromFoldable [(Tuple (TokenName(byteArrayFromIntArrayUnsafe [78,84,88])) fromString "1000000000000")]))]))), dataHash: Nothing })),(Tuple (TransactionInput { index: 0u, transactionId: (TransactionHash (byteArrayFromIntArrayUnsafe [176,62,129,229,99,92,2,251,34,236,14,217,71,239,59,160,190,68,197,128,41,232,218,154,8,244,229,57,229,199,54,236])) }) (TransactionOutput { address: (Address addr_test1qpkl55y6av6lvu9gfjkqr0cmtkxn7qcezgg0q3vr0m2huda6qh8x3elu6qa9t48ymn0dluh805ws2we38uxwxcnzyygq5jl946), amount: (Value (Coin fromString "1000000000") (NonAdaAsset(fromFoldable []))), dataHash: Nothing })),(Tuple (TransactionInput { index: 1u, transactionId: (TransactionHash (byteArrayFromIntArrayUnsafe [192,180,234,238,168,113,160,1,25,4,83,234,130,63,142,234,164,34,179,130,4,200,25,176,183,255,96,73,177,238,155,52])) }) (TransactionOutput { address: (Address addr_test1qpkl55y6av6lvu9gfjkqr0cmtkxn7qcezgg0q3vr0m2huda6qh8x3elu6qa9t48ymn0dluh805ws2we38uxwxcnzyygq5jl946), amount: (Value (Coin fromString "1644798") (NonAdaAsset(fromFoldable [(Tuple (CurrencySymbol(byteArrayFromIntArrayUnsafe [111,26,31,12,124,207,99,44,201,255,75,121,104,126,209,63,254,91,98,76,206,40,139,54,78,189,206,80])) (fromFoldable [(Tuple (TokenName(byteArrayFromIntArrayUnsafe [65,71,73,88])) fromString "1000000000000")]))]))), dataHash: Nothing })),(Tuple (TransactionInput { index: 0u, transactionId: (TransactionHash (byteArrayFromIntArrayUnsafe [232,115,57,87,201,123,17,116,136,146,14,182,104,229,249,225,188,213,102,53,42,27,18,93,76,175,167,103,204,64,226,148])) }) (TransactionOutput { address: (Address addr_test1qpkl55y6av6lvu9gfjkqr0cmtkxn7qcezgg0q3vr0m2huda6qh8x3elu6qa9t48ymn0dluh805ws2we38uxwxcnzyygq5jl946), amount: (Value (Coin fromString "2748766341") (NonAdaAsset(fromFoldable [(Tuple (CurrencySymbol(byteArrayFromIntArrayUnsafe [253,161,182,180,135,190,226,231,246,78,207,36,210,75,18,36,52,36,132,192,25,94,225,183,185,67,219,80])) (fromFoldable [(Tuple (TokenName(byteArrayFromIntArrayUnsafe [68,117,109,109,121,84,111,107,101,110])) fromString "72")]))]))), dataHash: Nothing })),(Tuple (TransactionInput { index: 0u, transactionId: (TransactionHash (byteArrayFromIntArrayUnsafe [245,59,122,40,33,117,55,116,240,74,248,36,150,8,24,236,171,182,122,183,182,17,64,30,116,72,19,10,27,252,161,14])) }) (TransactionOutput { address: (Address addr_test1qpkl55y6av6lvu9gfjkqr0cmtkxn7qcezgg0q3vr0m2huda6qh8x3elu6qa9t48ymn0dluh805ws2we38uxwxcnzyygq5jl946), amount: (Value (Coin fromString "983574068") (NonAdaAsset(fromFoldable []))), dataHash: Nothing })),(Tuple (TransactionInput { index: 0u, transactionId: (TransactionHash (byteArrayFromIntArrayUnsafe [252,115,129,158,0,14,89,75,214,249,109,50,99,210,255,101,55,185,123,234,240,255,27,3,124,97,210,29,141,131,236,206])) }) (TransactionOutput { address: (Address addr_test1qpkl55y6av6lvu9gfjkqr0cmtkxn7qcezgg0q3vr0m2huda6qh8x3elu6qa9t48ymn0dluh805ws2we38uxwxcnzyygq5jl946), amount: (Value (Coin fromString "980646113") (NonAdaAsset(fromFoldable []))), dataHash: Nothing }))])