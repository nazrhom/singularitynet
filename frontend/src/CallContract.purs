module CallContract
  ( ContractConfiguration
  , callClosePool
  , callCreatePool
  , callDepositPool
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
  , runContract
  , defaultSlotConfig
  , mkContractConfig
  )
import Contract.Numeric.NatRatio (fromNaturals, toRational)
import Contract.Numeric.Natural (Natural, fromBigInt, toBigInt)
import Contract.Prim.ByteArray
  ( byteArrayFromAscii
  , byteArrayToHex
  , hexToByteArray
  )
import Contract.Scripts (ed25519KeyHashFromBytes, ed25519KeyHashToBytes)
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
import ClosePool (closePoolContract)
import CreatePool (createBondedPoolContract)
import Serialization.Address (intToNetworkId)
import Data.BigInt (BigInt)
import Data.Int as Int
import Data.UInt as UInt
import Data.UInt (UInt)
import DepositPool (depositPoolContract)
import Effect.Aff (error)
import Effect.Exception (Error)
import Types (BondedPoolParams(BondedPoolParams), InitialBondedParams)
import Types.Rational (denominator, numerator) -- fix this with updated CTL

-- | Configuation needed to call contracts from JS.
type ContractConfiguration =
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
fromLogLevelStr "Trace" = pure Trace
fromLogLevelStr "Debug" = pure Debug
fromLogLevelStr "Info" = pure Info -- default
fromLogLevelStr "Warn" = pure Warn
fromLogLevelStr "Error" = pure Error
fromLogLevelStr _ = Nothing

buildContractConfig :: ContractConfiguration -> Aff (ContractConfig ())
buildContractConfig cfg = do
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
    , slotConfig: defaultSlotConfig
    , logLevel: logLevel
    , extraConfig: {}
    , wallet
    }
  where
  convertPort :: String -> Number -> Aff UInt
  convertPort name port =
    liftM (error $ "buildContractConfig: Invalid " <> name <> " port number")
      $ UInt.fromNumber' port

callCreatePool
  :: ContractConfiguration
  -> InitialBondedArgs
  -> Effect (Promise BondedPoolArgs)
callCreatePool cfg iba = Promise.fromAff do
  contractConfig <- buildContractConfig cfg
  ibp <- liftEither $ fromInitialBondedArgs iba
  bpp <- runContract contractConfig (createBondedPoolContract ibp)
  pure $ toBondedPoolArgs bpp

callDepositPool
  :: ContractConfiguration -> BondedPoolArgs -> Effect (Promise Unit)
callDepositPool = callWithBondedPoolArgs depositPoolContract

callClosePool
  :: ContractConfiguration -> BondedPoolArgs -> Effect (Promise Unit)
callClosePool = callWithBondedPoolArgs closePoolContract

callWithBondedPoolArgs
  :: (BondedPoolParams -> Contract () Unit)
  -> ContractConfiguration
  -> BondedPoolArgs
  -> Effect (Promise Unit)
callWithBondedPoolArgs contract cfg bpa = Promise.fromAff do
  contractConfig <- buildContractConfig cfg
  bpp <- liftEither $ fromBondedPoolArgs bpa
  runContract contractConfig (contract bpp)

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
          /\ byteArrayToHex (getTokenName bas.tokenName)
    , admin: byteArrayToHex $ ed25519KeyHashToBytes $ unwrap $ unwrap bpp.admin
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
    <$> (ed25519KeyHashFromBytes =<< hexToByteArray bpa.admin)
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