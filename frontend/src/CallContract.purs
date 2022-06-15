module CallContract
  ( SdkConfig
  , bondedCallContractAdminCloseExample1
  , bondedCallContractAdminDepositExample1
  , bondedCallContractCreatePoolExample1
  , bondedCallContractExample1
  , bondedCallContractUserStakeExample1
  , buildContractConfig
  , callCloseBondedPool
  , callCreateBondedPool
  , callDepositBondedPool
  , callUserStakeBondedPool
  ) where

import Contract.Prelude

import Contract.Monad
  ( Contract
  , ContractConfig
  , LogLevel
      ( Trace
      , Debug
      , Info
      , Warn
      , Error
      )
  , launchAff_
  , runContract
  , mkContractConfig
  )
import Contract.Numeric.NatRatio (fromNaturals, toRational)
import Contract.Numeric.Natural (Natural, fromBigInt, toBigInt)
import Contract.Numeric.Rational (denominator, numerator)
import Contract.Prim.ByteArray
  ( byteArrayFromAscii
  , byteArrayToHex
  , hexToByteArray
  )
import Contract.Value
  ( CurrencySymbol
  , getCurrencySymbol
  , getTokenName
  , mkCurrencySymbol
  , mkTokenName
  )
import Contract.Wallet (mkNamiWalletAff)
import Control.Promise (Promise)
import Control.Promise as Promise
import ClosePool (closeBondedPoolContract)
import CreatePool (createBondedPoolContract)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int as Int
import Data.UInt as UInt
import Data.UInt (UInt)
import DepositPool (depositBondedPoolContract)
import Effect.Aff (delay, error)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Record (merge)
import Serialization.Address (intToNetworkId)
import Serialization.Hash (ed25519KeyHashFromBytes, ed25519KeyHashToBytes)
import Types (BondedPoolParams(BondedPoolParams), InitialBondedParams)
import Types.CborBytes (cborBytesToHex)
import Types.RawBytes (hexToRawBytes, rawBytesToHex)
import UserStake (userStakeBondedPoolContract)

-- | Configuation needed to call contracts from JS.
type SdkConfig =
  { serverHost :: String
  , serverPort :: Number -- converts to UInt
  , serverSecure :: Boolean
  , ogmiosHost :: String
  , ogmiosPort :: Number -- converts to UInt
  , ogmiosSecure :: Boolean
  , datumCacheHost :: String
  , datumCachePort :: Number -- converts to UInt
  , datumCacheSecure :: Boolean
  , networkId :: Number -- converts to Int
  , logLevel :: String -- "Trace", "Debug", "Info", "Warn", "Error"
  }

type InitialBondedArgs =
  { iterations :: BigInt -- Natural
  , start :: BigInt -- like POSIXTime
  , end :: BigInt -- like POSIXTime
  , userLength :: BigInt -- like POSIXTime
  , bondingLength :: BigInt -- like POSIXTime
  , interest :: Tuple BigInt BigInt -- Rational
  , minStake :: BigInt -- Natural
  , maxStake :: BigInt -- Natural
  , bondedAssetClass ::
      Tuple String String -- AssetClass ~ Tuple CBORCurrencySymbol ASCIITokenName
  }

type BondedPoolArgs =
  { iterations :: BigInt -- Natural
  , start :: BigInt -- like POSIXTime
  , end :: BigInt -- like POSIXTime
  , userLength :: BigInt -- like POSIXTime
  , bondingLength :: BigInt -- like POSIXTime
  , interest :: Tuple BigInt BigInt -- Rational
  , minStake :: BigInt -- Natural
  , maxStake :: BigInt -- Natural
  , bondedAssetClass ::
      Tuple String String -- AssetClass ~ Tuple CBORCurrencySymbol ASCIITokenName
  , admin :: String -- PaymentPubKeyHash
  , nftCs :: String -- CurrencySymbol
  , assocListCs :: String -- CurrencySymbol
  }

fromLogLevelStr :: String -> Maybe LogLevel
fromLogLevelStr = case _ of
  "Trace" -> pure Trace
  "Debug" -> pure Debug
  "Info" -> pure Info -- default
  "Warn" -> pure Warn
  "Error" -> pure Error
  _ -> Nothing

buildContractConfig :: SdkConfig -> Effect (Promise (ContractConfig ()))
buildContractConfig cfg = Promise.fromAff $ do
  serverPort <- convertPort "server" cfg.serverPort
  ogmiosPort <- convertPort "ogmios" cfg.ogmiosPort
  datumCachePort <- convertPort "datum cache" cfg.datumCachePort
  networkIdInt <- liftM (error "buildContractConfig: Invalid network id Int")
    $ Int.fromNumber cfg.networkId
  networkId <- liftM (error "buildContractConfig: Invalid network id")
    $ intToNetworkId networkIdInt
  wallet <- Just <$> mkNamiWalletAff
  logLevel <- liftM (error "buildContractConfig: Invalid LogLevel")
    $ fromLogLevelStr cfg.logLevel
  mkContractConfig $ wrap
    { ogmiosConfig:
        { port: ogmiosPort
        , host: cfg.ogmiosHost
        , secure: cfg.ogmiosSecure
        }
    , datumCacheConfig:
        { port: datumCachePort
        , host: cfg.datumCacheHost
        , secure: cfg.datumCacheSecure
        }
    , ctlServerConfig:
        { port: serverPort
        , host: cfg.serverHost
        , secure: cfg.serverSecure
        }
    , networkId
    , logLevel: logLevel
    , extraConfig: {}
    , wallet
    }
  where
  convertPort :: String -> Number -> Aff UInt
  convertPort name port =
    liftM (error $ "buildContractConfig: Invalid " <> name <> " port number")
      $ UInt.fromNumber' port

callCreateBondedPool
  :: ContractConfig ()
  -> InitialBondedArgs
  -> Effect (Promise BondedPoolArgs)
callCreateBondedPool cfg iba = Promise.fromAff do
  ibp <- liftEither $ fromInitialBondedArgs iba
  bpp <- runContract cfg $ createBondedPoolContract ibp
  pure $ toBondedPoolArgs bpp

callDepositBondedPool
  :: ContractConfig () -> BondedPoolArgs -> Effect (Promise Unit)
callDepositBondedPool = callWithBondedPoolArgs depositBondedPoolContract

callCloseBondedPool
  :: ContractConfig () -> BondedPoolArgs -> Effect (Promise Unit)
callCloseBondedPool = callWithBondedPoolArgs closeBondedPoolContract

callUserStakeBondedPool
  :: ContractConfig () -> BondedPoolArgs -> BigInt -> Effect (Promise Unit)
callUserStakeBondedPool cfg bpa bi = Promise.fromAff $ runContract cfg do
  bpp <- liftEither $ fromBondedPoolArgs bpa
  nat <- liftM (error "callUserStakeBondedPool: Invalid natural number")
    $ fromBigInt bi
  userStakeBondedPoolContract bpp nat

callWithBondedPoolArgs
  :: (BondedPoolParams -> Contract () Unit)
  -> ContractConfig ()
  -> BondedPoolArgs
  -> Effect (Promise Unit)
callWithBondedPoolArgs contract cfg bpa = Promise.fromAff
  $ runContract cfg
  <<< contract
  =<< liftEither (fromBondedPoolArgs bpa)

fromInitialBondedArgs
  :: InitialBondedArgs -> Either Error InitialBondedParams
fromInitialBondedArgs iba = do
  iterations <- toNat "iteration" iba.iterations
  interestNum <- toNat "interest numerator" $ fst iba.interest
  interestDen <- toNat "interest denominator" $ snd iba.interest
  interest <- note (error "fromInitialBondedArgs: Invalid Rational")
    (toRational <$> fromNaturals interestNum interestDen)
  minStake <- toNat "minStake" iba.minStake
  maxStake <- toNat "maxStake" iba.maxStake
  currencySymbol <- note (error "fromInitialBondedArgs: Invalid CS")
    $ mkCurrencySymbol
    =<< hexToByteArray (fst iba.bondedAssetClass)
  tokenName <- note (error "fromInitialBondedArgs: Invalid TN")
    $ mkTokenName
    =<< byteArrayFromAscii (snd iba.bondedAssetClass)
  pure $ wrap
    { iterations
    , start: iba.start
    , end: iba.end
    , userLength: iba.userLength
    , bondingLength: iba.bondingLength
    , interest
    , minStake
    , maxStake
    , bondedAssetClass: wrap { currencySymbol, tokenName }
    }
  where
  toNat :: String -> BigInt -> Either Error Natural
  toNat name bint =
    note
      ( error
          $
            "fromInitialBondedArgs: Could not convert "
          <> name
          <> " to Natural"
      ) $ fromBigInt bint

toBondedPoolArgs :: BondedPoolParams -> BondedPoolArgs
toBondedPoolArgs (BondedPoolParams bpp@{ interest: i, bondedAssetClass: bas' }) =
  let
    bas = unwrap bas'
  in
    { iterations: toBigInt bpp.iterations
    , start: bpp.start
    , end: bpp.end
    , userLength: bpp.userLength
    , bondingLength: bpp.bondingLength
    , interest: numerator i /\ denominator i
    , minStake: toBigInt bpp.minStake
    , maxStake: toBigInt bpp.maxStake
    , bondedAssetClass:
        byteArrayToHex (getCurrencySymbol bas.currencySymbol)
          /\ cborBytesToHex (getTokenName bas.tokenName)
    , admin: rawBytesToHex $ ed25519KeyHashToBytes $ unwrap $ unwrap bpp.admin
    , nftCs: byteArrayToHex $ getCurrencySymbol bpp.nftCs
    , assocListCs: byteArrayToHex $ getCurrencySymbol bpp.assocListCs
    }

fromBondedPoolArgs :: BondedPoolArgs -> Either Error BondedPoolParams
fromBondedPoolArgs bpa = do
  iterations <- toNat "iteration" bpa.iterations
  interestNum <- toNat "interest numerator" $ fst bpa.interest
  interestDen <- toNat "interest denominator" $ snd bpa.interest
  interest <- note (error "fromBondedPoolArgs: Invalid Rational")
    (toRational <$> fromNaturals interestNum interestDen)
  minStake <- toNat "minStake" bpa.minStake
  maxStake <- toNat "maxStake" bpa.maxStake
  admin <- note (error $ "fromBondedPoolArgs: Invalid admin: " <> bpa.admin)
    $ wrap
    <<< wrap
    <$> (ed25519KeyHashFromBytes =<< hexToRawBytes bpa.admin)
  currencySymbol <- toCs $ fst bpa.bondedAssetClass
  tokenName <- note (error "fromBondedPoolArgs: Invalid TN")
    $ mkTokenName
    =<< byteArrayFromAscii (snd bpa.bondedAssetClass)
  nftCs <- toCs bpa.nftCs
  assocListCs <- toCs bpa.assocListCs
  pure $ wrap
    { iterations
    , start: bpa.start
    , end: bpa.end
    , userLength: bpa.userLength
    , bondingLength: bpa.bondingLength
    , interest
    , minStake
    , maxStake
    , admin
    , bondedAssetClass: wrap { currencySymbol, tokenName }
    , nftCs
    , assocListCs
    }
  where
  toNat :: String -> BigInt -> Either Error Natural
  toNat name bint =
    note
      ( error
          $
            "fromBondedPoolArgs: Could not convert "
          <> name
          <> " to Natural"
      ) $ fromBigInt bint

  toCs :: String -> Either Error CurrencySymbol
  toCs str = note (error "fromBondedPoolArgs: Invalid CS")
    $ mkCurrencySymbol
    =<< hexToByteArray str

------------------------ Used for Testing SDK ----------------------------------
defaultSdkConfig :: SdkConfig
defaultSdkConfig =
  { serverHost: "localhost"
  , serverPort: 8081.0 -- converts to UInt
  , serverSecure: false
  , ogmiosHost: "localhost"
  , ogmiosPort: 1337.0 -- converts to UInt
  , ogmiosSecure: false
  , datumCacheHost: "localhost"
  , datumCachePort: 9999.0 -- converts to UInt
  , datumCacheSecure: false
  , networkId: zero -- converts to Int
  , logLevel: "Info" -- "Trace", "Debug", "Info", "Warn", "Error"
  }

-- Hardcoded initial parameters for testing.
testInitialBondedArgs :: Maybe InitialBondedArgs
testInitialBondedArgs = do
  -- Whilst some of these numbers are small, I'd rather use fromString to be safe
  -- in case we switch to larger numbers.
  iterations <- BigInt.fromString "3"
  start <- BigInt.fromString "1000"
  end <- BigInt.fromString "2000"
  userLength <- BigInt.fromString "100"
  bondingLength <- BigInt.fromString "4"
  interestNum <- BigInt.fromString "10"
  interestDen <- BigInt.fromString "100"
  let interest = interestNum /\ interestDen
  minStake <- BigInt.fromString "1"
  maxStake <- BigInt.fromString "10000"
  let
    bondedCs = "6f1a1f0c7ccf632cc9ff4b79687ed13ffe5b624cce288b364ebdce50"
    bondedTn = "AGIX"
    bondedAssetClass = bondedCs /\ bondedTn
  pure
    { iterations
    , start
    , end
    , userLength
    , bondingLength
    , interest
    , minStake
    , maxStake
    , bondedAssetClass
    }

-- Hardcoded to pass persistent parameters onto subsequent calls.
testBondedPoolArgs :: Maybe BondedPoolArgs
testBondedPoolArgs = do
  iba <- testInitialBondedArgs
  let
    admin = "7df2b92eabfe7ee15d5bd05daf11572f3fcae3b7026851ab5d9d28e6"
    assocListCs = "c355f15087c2a3358c197bac069370fa8dee57c07604b6862378165b"
    nftCs = "7dbfb68c955a679825e39c50a4c495aca22c3c2a2888836d6c5e32ca"
  pure $ merge iba
    { "admin": admin, "assocListCs": assocListCs, "nftCs": nftCs }

-- Bonded example: admin deposit, user head stake, admin deposit and admin close.
-- This doesn't seem to work, instead, we need to break down the example into
-- separate effects. See `bondedCallContractCreatePoolExample1`
-- `bondedCallContractUserStakeExample1`, `bondedCallContractAdminDepositExample1`
-- and `bondedCallContractAdminCloseExample1`.
bondedCallContractExample1 :: Effect Unit
bondedCallContractExample1 = launchAff_ do
  adminCfg <- Promise.toAffE $ buildContractConfig defaultSdkConfig
  iba <-
    liftM
      ( error
          "bondedCallContractExample1: Could not create \
          \InitialBondedArgs"
      )
      $ testInitialBondedArgs
  log "STARTING AS ADMIN"
  -- Create pool
  bondedParams <- Promise.toAffE $ callCreateBondedPool adminCfg iba
  log "SWITCH WALLETS NOW - CHANGE TO USER 1"
  delay $ wrap $ Int.toNumber 100_00
  -- User 1 deposits
  userCfg <- Promise.toAffE $ buildContractConfig defaultSdkConfig
  userStake <- liftM (error "bondedCallContractExample1: Cannot create BigInt")
    $ BigInt.fromString "10"
  Promise.toAffE $ callUserStakeBondedPool userCfg bondedParams userStake
  log "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
  delay $ wrap $ Int.toNumber 100_00
  -- Admin deposit
  Promise.toAffE $ callDepositBondedPool adminCfg bondedParams
  log "DON'T SWITCH WALLETS - STAY AS ADMIN"
  delay $ wrap $ Int.toNumber 100_00
  -- Admin close
  Promise.toAffE $ callCloseBondedPool adminCfg bondedParams

-- These examples work when called separately. An example can be found inside
-- `exe/Main.purs`
bondedCallContractCreatePoolExample1 :: Effect Unit
bondedCallContractCreatePoolExample1 = launchAff_ do
  adminCfg <- Promise.toAffE $ buildContractConfig defaultSdkConfig
  iba <-
    liftM
      ( error
          "bondedCallContractCreatePoolExample1: Could not create \
          \InitialBondedArgs"
      )
      $ testInitialBondedArgs
  log "STARTING AS ADMIN"
  -- Create pool
  bondedParams <- Promise.toAffE $ callCreateBondedPool adminCfg iba
  log $ "admin: " <> show bondedParams.admin
  log $ "assocListCs: " <> show bondedParams.assocListCs
  log $ "nftCs: " <> show bondedParams.nftCs
  log "SWITCH WALLETS NOW - CHANGE TO USER 1"

bondedCallContractUserStakeExample1 :: Effect Unit
bondedCallContractUserStakeExample1 = launchAff_ do
  userCfg <- Promise.toAffE $ buildContractConfig defaultSdkConfig
  userStake <-
    liftM
      ( error
          "bondedCallContractUserStakeExample1: Cannot \
          \create BigInt"
      )
      $ BigInt.fromString "10"
  bondedParams <- liftM
    ( error
        "bondedCallContractUserStakeExample1: Cannot \
        \ get bonded pool parameters"
    )
    testBondedPoolArgs
  Promise.toAffE $ callUserStakeBondedPool userCfg bondedParams userStake
  log "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"

bondedCallContractAdminDepositExample1 :: Effect Unit
bondedCallContractAdminDepositExample1 = launchAff_ do
  adminCfg <- Promise.toAffE $ buildContractConfig defaultSdkConfig
  bondedParams <- liftM
    ( error
        "bondedCallContractAdminDepositExample1: Cannot \
        \ get bonded pool parameters"
    )
    testBondedPoolArgs
  -- Admin deposit
  Promise.toAffE $ callDepositBondedPool adminCfg bondedParams
  log "DON'T SWITCH WALLETS - STAY AS ADMIN"

bondedCallContractAdminCloseExample1 :: Effect Unit
bondedCallContractAdminCloseExample1 = launchAff_ do
  adminCfg <- Promise.toAffE $ buildContractConfig defaultSdkConfig
  bondedParams <- liftM
    ( error
        "bondedCallContractAdminCloseExample1: Cannot \
        \ get bonded pool parameters"
    )
    testBondedPoolArgs
  -- Admin close
  Promise.toAffE $ callCloseBondedPool adminCfg bondedParams
