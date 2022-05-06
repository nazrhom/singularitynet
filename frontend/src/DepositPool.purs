module DepositPool (depositPoolContract) where

import Contract.Prelude

import Data.Argonaut (decodeJson, parseJson)
import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , validatorHashEnterpriseAddress
  )
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftContractE
  , liftedE'
  , liftedM
  )
import Contract.PlutusData (PlutusData, Datum(Datum), toData, unitDatum)
import Contract.Prim.ByteArray (byteArrayToHex, byteArrayFromAscii)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash, MintingPolicy)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustPayToScript
  , mustSpendScriptOutput
  , mustMintValue
  )
import Contract.Utxos (utxosAt)
import Contract.Value
  ( adaSymbol
  , adaToken
  , singleton
  , mkTokenName
  , scriptCurrencySymbol
  )
import Data.Array (head)
import Data.Map (toUnfoldable)
import Scripts.BondedPoolValidator (mkBondedPoolValidator)
import Settings (hardCodedParams)
import Types
  ( BondedStakingAction(AdminAct)
  , BondedStakingDatum(StateDatum)
  , PoolInfo(PoolInfo)
  )
import Types.Redeemer (Redeemer(Redeemer))
import Utils (big, nat, logInfo_)

-- Deposits a certain amount in the pool
depositPoolContract :: PoolInfo -> Contract () Unit
depositPoolContract (PoolInfo { stateNftCs, assocListCs, poolAddr }) = do
  -- Fetch information related to the pool
  -- Get network ID and admin's PKH
  logInfo_ "Pool address" poolAddr
  networkId <- getNetworkId
  adminPkh <- liftedM "depositPoolContract: Cannot get admin's pkh"
    ownPaymentPubKeyHash
  logInfo_ "Admin PaymentPubKeyHash" adminPkh
  -- Get the (Nami) wallet address
  adminAddr <- liftedM "depositPoolContract: Cannot get wallet Address"
    getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "depositPoolContract: Cannot get user Utxos" $ utxosAt adminAddr
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM "depositPoolContract: Cannot get pool's utxos at pool address" $
      utxosAt poolAddr
  -- Fix this to find the UTXO, not the head:
  poolTxInput <-
    liftContractM "depositPoolContract: Cannot get head Utxo for bonded pool"
      $ fst
      <$> (head $ toUnfoldable $ unwrap bondedPoolUtxos)
  logInfo_ "Pool's UTXO" poolTxInput
  -- We define the parameters of the pool
  params <- liftContractM "depositPoolContract: Failed to create parameters" $
    hardCodedParams adminPkh stateNftCs assocListCs
  logInfo_ "toData Pool Parameters" $ toData params
  -- Get the bonded pool validator and hash
  validator <- liftedE' "depositPoolContract: Cannot create validator" $
    mkBondedPoolValidator params
  valHash <- liftedM "depositPoolContract: Cannot hash validator"
    $ validatorHash validator
  -- For whatever reason, minting a dummy token is required to pay to script.
  -- This is a CTL bug.
  dummyMp :: MintingPolicy <- liftContractE
    ( wrap <$>
        ( decodeJson =<< parseJson
            "\"59094c010000323232332233223332223233322233322233333333222222223322333332222233332222333222332233223322333222332233223322332233223232323232323232323232323232323232323232323232323233500101122031122223005330033004002300600125335302c001104d13501835304c33573892010250640004d4988c8c8c8c8c8c8cccd5cd19b8735573aa00a90001280112803a4c26603ca002a0042600c6ae8540084c050d5d09aba25001135573ca00226ea80084d405d262323232323232323232323232323232323232323232323333573466e1cd55cea80aa40004a0044a02e9309999999999817a800a8012801a8022802a8032803a8042804a805099a81080b1aba15012133502001635742a0202666aa032eb94060d5d0a8070999aa80c3ae501735742a018266a03a0426ae8540284cd4070cd54078085d69aba15008133501675a6ae8540184cd4069d71aba150041335019335501b75c0346ae8540084c080d5d09aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea80084d40592623232323232323333573466e1cd55cea802a40004a0044a00e930998102800a8010980b9aba1500213005357426ae8940044d55cf280089baa0021350154988c8c8c8c8c8c8c8c8cccd5cd19b8735573aa00e90001280112804a4c2666046a002a004a006260106ae8540104ccd54029d728049aba15002133500775c6ae84d5d1280089aba25001135573ca00226ea80084d40512623232323232323333573466e1cd55cea802a40004a0044a00e930998112800a8010980a1aba150021335005012357426ae8940044d55cf280089baa002135013498488c8c8c8c8c8c8cccd5cd19b87500448000940089401126135025500113006357426aae79400c4cccd5cd19b875001480089408c9401126135573aa00226ea80084d404d261335500175ceb444888c8c8c004dd58019a80090008918009aa82591191919191919191999aab9f0085504c25300212001051350022200135001220023555505212223300321300a357440124266a09ea00aa600624002266aa09ea002a004260106aae7540084c018d55cf280089aba10011223232323232323333573466e1cd55cea802a40004a0044a00e93099a8122800a801099a8038031aba150021335007005357426ae8940044d55cf280089baa002135010498488c8c8c8c8c8c8cccd5cd19b8735573aa00a90001280112803a4c266a04ea002a004266a01000c6ae8540084c020d5d09aba25001135573ca00226ea80084d403d261223232323232323333573466e1cd55cea802a40004a0044a00e93099a8122800a801099a8038031aba1500213007357426ae8940044d55cf280089baa00213500e498488c8c8c8c8c8c8c8cccd5cd19b87500548010940b4940092613333573466e1d4011200225002250044984d40b140044c018d5d09aab9e500313333573466e1d400520002502a250044984d55cea80089baa00213500d4988c8c8c8cccd5cd19b875002480088094940092613333573466e1d400520002023250034984d55ce9baa00213500b498488c8c8c004dd60019a80090008918009aa822111999aab9f00125042233504130063574200460066ae8800810c800444888c8c8c8c8c8c8cccd5cd19b8735573aa00a90001280112803a4c266aa08ca002a0042600e6ae8540084c014d5d09aba25001135573ca00226ea80084d402926232323232323232323232323232323333573466e1d4029200625002250044984c0cd40044c038d5d09aab9e500b13333573466e1d401d200425002250044984c0b940044c030d5d09aab9e500813333573466e1d4011200225002250044984c0a940044c02cd5d09aab9e500513333573466e1d4005200025003250064984d55cea80189814280089bae357426aae7940044dd500109a803a4c4646464646464646464646464646464646464646464646464646666ae68cdc3a80aa4018408a4a0049309999ab9a3370ea028900510229280124c26666ae68cdc3a809a40104a0044a00c9309981fa800a80109bae35742a00426eb4d5d09aba25001135573ca02426666ae68cdc3a8072400c4a0044a00c9309981da800a80109bae35742a00426eb8d5d09aba25001135573ca01a26666ae68cdc3a804a40084a0044a00c9309981d2800a801098069aba150021375c6ae84d5d1280089aab9e500813333573466e1d4011200225002250044984c0d940044c020d5d09aab9e500513333573466e1d4005200025003250064984d55cea801898182800898021aba135573ca00226ea80084d40192623232323232323232323232323333573466e1d4021200225002250084984ccc0ed40054009400c4dd69aba150041375a6ae8540084dd69aba135744a00226ae8940044d55cf280289999ab9a3370ea0029000128019280324c26aae75400c4c0d140044c010d5d09aab9e50011375400426a00a93119191919191919191999ab9a3370ea0089001128011280224c26072a00226eb8d5d09aab9e500513333573466e1d4005200025003250064984d55cea8018981b280089bae357426aae7940044dd500109a80224c46464646464646666ae68cdc39aab9d500548000940089401d26133029500150021300635742a00426eb4d5d09aba25001135573ca00226ea80084d400d2623232323333573466e1cd55cea801240004a0044a0089309bae357426aae7940044dd500109a80124c24c4424660020060044002444444444424666666666600201601401201000e00c00a00800600440024424660020060044002444246660020080060044002442466002006004400224244600400622440022400224424660020060042400224424660020060042400224424660020060042400224400424400240022424446006008224440042244400224002424444600800a424444600600a424444600400a424444600200a40024424660020060044002424444444600e01044244444446600c012010424444444600a010244444440082444444400644244444446600401201044244444446600201201040024244600400644424466600200a008006400242446004006424460020064002224a0082244004244244660020080062400224002400222442466002006004224002224646002002446600660040040022222466a0044246600246a00644600400646a00644600200600224646460020024466006600400400244246a6008246a60080066a006002003\""
        )
    )
  dummyCs <- liftedM "Cannot get dummy cs" $ scriptCurrencySymbol dummyMp
  dummyTn <- liftContractM "Cannot make dummy token"
    $ mkTokenName
    =<< byteArrayFromAscii "DummyToken"
  let
    depositValue = singleton adaSymbol adaToken $ big 5_000_000
    -- Minting dummy value due to CTL issue
    dummyMintValue = singleton dummyCs dummyTn $ big 12
    scriptAddr = validatorHashEnterpriseAddress networkId valHash
  logInfo_ "BondedPool Validator's address" scriptAddr
  let
    -- We can hardcode the state for now. We should actually fetch the datum
    -- from Ogmios, update it properly and then submit it
    bondedStateDatum = Datum $ toData $ StateDatum
      { maybeEntryName: Nothing
      , sizeLeft: nat 100
      }
    -- We build the redeemer
    redeemerData = toData $ AdminAct { sizeLeft: nat 100 }
    redeemer = Redeemer redeemerData

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap adminUtxos
      , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
      , ScriptLookups.mintingPolicy dummyMp
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [ mustPayToScript valHash unitDatum depositValue
        , mustMintValue dummyMintValue
        -- , mustSpendScriptOutput poolTxInput redeemer
        ]

  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "depositPoolContract: Cannot balance, reindex redeemers, attach datums/\
      \redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_ "depositPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash
