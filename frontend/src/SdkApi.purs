module SdkApi
  ( SdkConfig
  , SdkServerConfig
  , buildContractConfig
  -- Bonded
  , BondedPoolArgs
  , InitialBondedArgs
  , callCloseBondedPool
  , callCreateBondedPool
  , callDepositBondedPool
  , callUserStakeBondedPool
  , callUserWithdrawBondedPool
  -- Unbonded
  , UnbondedPoolArgs
  , InitialUnbondedArgs
  , callCloseUnbondedPool
  , callCreateUnbondedPool
  , callDepositUnbondedPool
  , callUserStakeUnbondedPool
  , callUserWithdrawUnbondedPool
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
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
  , mkContractConfig
  )
import Contract.Numeric.NatRatio (fromNaturals, toRational)
import Contract.Numeric.Natural (Natural, fromBigInt, toBigInt)
import Contract.Numeric.Rational (Rational, denominator, numerator)
import Contract.Prim.ByteArray
  ( byteArrayFromAscii
  , byteArrayToHex
  , hexToByteArray
  )
import Contract.Value
  ( CurrencySymbol
  , TokenName
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
import Data.Int as Int
import Data.UInt as UInt
import Data.UInt (UInt)
import DepositPool (depositBondedPoolContract)
import Effect.Aff (error)
import Effect.Exception (Error)
import Serialization.Address (intToNetworkId)
import Serialization.Hash (ed25519KeyHashFromBytes, ed25519KeyHashToBytes)
import Types
  ( AssetClass(AssetClass)
  , BondedPoolParams(BondedPoolParams)
  , InitialBondedParams
  )
import Types.RawBytes (hexToRawBytes, rawBytesToHex)
import UnbondedStaking.Types
  ( UnbondedPoolParams(UnbondedPoolParams)
  , InitialUnbondedParams
  )
import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
import UnbondedStaking.CreatePool (createUnbondedPoolContract)
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import UnbondedStaking.UserWithdraw (userWithdrawUnbondedPoolContract)
import UserStake (userStakeBondedPoolContract)
import UserWithdraw (userWithdrawBondedPoolContract)

-- | Configuation needed to call contracts from JS.
type SdkConfig =
  { ctlServerConfig :: SdkServerConfig
  , ogmiosConfig :: SdkServerConfig
  , datumCacheConfig :: SdkServerConfig
  , networkId :: Number -- converts to Int
  , logLevel :: String -- "Trace", "Debug", "Info", "Warn", "Error"
  }

type SdkServerConfig =
  { host :: String
  , port :: Number -- converts to UInt
  , secure :: Boolean
  }

fromSdkLogLevel :: String -> Maybe LogLevel
fromSdkLogLevel = case _ of
  "Trace" -> pure Trace
  "Debug" -> pure Debug
  "Info" -> pure Info -- default
  "Warn" -> pure Warn
  "Error" -> pure Error
  _ -> Nothing

fromSdkServerConfig
  :: String
  -> SdkServerConfig
  -> Either Error { port :: UInt, host :: String, secure :: Boolean }
fromSdkServerConfig serviceName conf@{ host, secure } = do
  port <-
    note (error $ "invalid " <> serviceName <> " port number")
      $ UInt.fromNumber' conf.port
  pure { port, host, secure }

buildContractConfig :: SdkConfig -> Effect (Promise (ContractConfig ()))
buildContractConfig cfg = Promise.fromAff $ do
  ctlServerConfig <- liftEither $ fromSdkServerConfig "ctl-server"
    cfg.ctlServerConfig
  ogmiosConfig <- liftEither $ fromSdkServerConfig "ogmios" cfg.ogmiosConfig
  datumCacheConfig <- liftEither $ fromSdkServerConfig "ogmios-datum-cache"
    cfg.datumCacheConfig
  networkIdInt <- liftM (errorWithContext "invalid `NetworkId`")
    $ Int.fromNumber cfg.networkId
  networkId <- liftM (errorWithContext "invalid `NetworkId`")
    $ intToNetworkId networkIdInt
  wallet <- Just <$> mkNamiWalletAff
  logLevel <- liftM (errorWithContext "invalid `LogLevel`")
    $ fromSdkLogLevel cfg.logLevel
  mkContractConfig $ wrap
    { ogmiosConfig
    , datumCacheConfig
    , ctlServerConfig
    , logLevel
    , networkId
    , wallet
    , extraConfig: {}
    }
  where
  errorWithContext :: String -> Error
  errorWithContext msg = error $ "buildContractConfig: " <> msg

callWithArgs
  :: forall (a :: Type) (b :: Type)
   . (a -> Either Error b)
  -> (b -> Contract () Unit)
  -> ContractConfig ()
  -> a
  -> Effect (Promise Unit)
callWithArgs f contract cfg args = Promise.fromAff
  $ runContract cfg
  <<< contract
  =<< liftEither (f args)

toSdkAssetClass :: AssetClass -> Tuple String String
toSdkAssetClass (AssetClass ac) =
  byteArrayToHex (getCurrencySymbol ac.currencySymbol)
    /\ byteArrayToHex (getTokenName ac.tokenName)

toSdkInterest :: Rational -> Tuple BigInt BigInt
toSdkInterest i = numerator i /\ denominator i

toSdkAdmin :: PaymentPubKeyHash -> String
toSdkAdmin = rawBytesToHex <<< ed25519KeyHashToBytes <<< unwrap <<< unwrap

toSdkCurrencySymbol :: CurrencySymbol -> String
toSdkCurrencySymbol = byteArrayToHex <<< getCurrencySymbol

fromSdkNat :: String -> String -> BigInt -> Either Error Natural
fromSdkNat context name bint = note (error msg) $ fromBigInt bint
  where
  msg :: String
  msg = context <> ": Could not convert " <> name <> " to `Natural`"

fromSdkAssetClass :: String -> String /\ String -> Either Error AssetClass
fromSdkAssetClass context (currencySymbol /\ tokenName) = map wrap
  $ { currencySymbol: _, tokenName: _ }
  <$> fromSdkCurrencySymbol context currencySymbol
  <*> fromSdkTokenName context tokenName

fromSdkTokenName :: String -> String -> Either Error TokenName
fromSdkTokenName context tokenName = note (errorWithMsg context "`TokenName`")
  $ mkTokenName
  =<< byteArrayFromAscii tokenName

fromSdkCurrencySymbol :: String -> String -> Either Error CurrencySymbol
fromSdkCurrencySymbol context currencySymbol =
  note (errorWithMsg context "`CurrencySymbol`") $ mkCurrencySymbol =<<
    hexToByteArray currencySymbol

fromSdkInterest :: String -> BigInt /\ BigInt -> Either Error Rational
fromSdkInterest context (numerator /\ denominator) = do
  interestNum <- fromSdkNat context "interest numerator" numerator
  interestDen <- fromSdkNat context "interest denominator" denominator
  note (error msg) $ toRational <$> fromNaturals interestNum interestDen
  where
  msg :: String
  msg = context <> ": invalid `Rational`"

fromSdkAdmin :: String -> String -> Either Error PaymentPubKeyHash
fromSdkAdmin context admin = note (error msg)
  $ wrap
  <<< wrap
  <$> (ed25519KeyHashFromBytes =<< hexToRawBytes admin)
  where
  msg :: String
  msg = context <> ": invalid admin"

errorWithMsg :: String -> String -> Error
errorWithMsg context name = error $ context <> ": invalid " <> name

--Bonded------------------------------------------------------------------------

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

callCreateBondedPool
  :: ContractConfig ()
  -> InitialBondedArgs
  -> Effect (Promise BondedPoolArgs)
callCreateBondedPool cfg iba = Promise.fromAff do
  ibp <- liftEither $ fromInitialBondedArgs iba
  { bondedPoolParams: bpp } <- runContract cfg $ createBondedPoolContract ibp
  pure $ toBondedPoolArgs bpp

callDepositBondedPool
  :: ContractConfig ()
  -> BondedPoolArgs
  -> BigInt
  -> Array Int
  -> Effect (Promise (Array Int))
callDepositBondedPool cfg bpa bi arr = Promise.fromAff $ runContract cfg do
  upp <- liftEither $ fromBondedPoolArgs bpa
  nat <- liftM (error "callDepositBondedPool: Invalid natural number")
    $ fromBigInt bi
  depositBondedPoolContract upp nat arr

callCloseBondedPool
  :: ContractConfig ()
  -> BondedPoolArgs
  -> BigInt
  -> Array Int
  -> Effect (Promise (Array Int))
callCloseBondedPool cfg bpa bi arr = Promise.fromAff $ runContract cfg do
  upp <- liftEither $ fromBondedPoolArgs bpa
  nat <- liftM (error "callCloseBondedPool: Invalid natural number")
    $ fromBigInt bi
  closeBondedPoolContract upp nat arr

callUserStakeBondedPool
  :: ContractConfig () -> BondedPoolArgs -> BigInt -> Effect (Promise Unit)
callUserStakeBondedPool cfg bpa bi = Promise.fromAff $ runContract cfg do
  bpp <- liftEither $ fromBondedPoolArgs bpa
  nat <- liftM (error "callUserStakeBondedPool: Invalid natural number")
    $ fromBigInt bi
  _ <- userStakeBondedPoolContract bpp nat
  pure unit

callUserWithdrawBondedPool
  :: ContractConfig () -> BondedPoolArgs -> Effect (Promise Unit)
callUserWithdrawBondedPool =
  callWithBondedPoolArgs $ (pure unit <* _) <<< userWithdrawBondedPoolContract

callWithBondedPoolArgs
  :: (BondedPoolParams -> Contract () Unit)
  -> ContractConfig ()
  -> BondedPoolArgs
  -> Effect (Promise Unit)
callWithBondedPoolArgs contract cfg = callWithArgs fromBondedPoolArgs contract
  cfg

fromInitialBondedArgs
  :: InitialBondedArgs -> Either Error InitialBondedParams
fromInitialBondedArgs iba = do
  iterations <- fromSdkNat' "iteration" iba.iterations
  interest <- fromSdkInterest context iba.interest
  minStake <- fromSdkNat' "minStake" iba.minStake
  maxStake <- fromSdkNat' "maxStake" iba.maxStake
  bondedAssetClass <- fromSdkAssetClass context iba.bondedAssetClass
  pure $ wrap
    { iterations
    , start: iba.start
    , end: iba.end
    , userLength: iba.userLength
    , bondingLength: iba.bondingLength
    , interest
    , minStake
    , maxStake
    , bondedAssetClass
    }
  where
  fromSdkNat' :: String -> BigInt -> Either Error Natural
  fromSdkNat' name = fromSdkNat context name

  context :: String
  context = "fromInitialBondedArgs"

toBondedPoolArgs :: BondedPoolParams -> BondedPoolArgs
toBondedPoolArgs (BondedPoolParams bpp) =
  { iterations: toBigInt bpp.iterations
  , start: bpp.start
  , end: bpp.end
  , userLength: bpp.userLength
  , bondingLength: bpp.bondingLength
  , interest: toSdkInterest bpp.interest
  , minStake: toBigInt bpp.minStake
  , maxStake: toBigInt bpp.maxStake
  , bondedAssetClass: toSdkAssetClass bpp.bondedAssetClass
  , admin: toSdkAdmin bpp.admin
  , nftCs: toSdkCurrencySymbol bpp.nftCs
  , assocListCs: toSdkCurrencySymbol bpp.assocListCs
  }

fromBondedPoolArgs :: BondedPoolArgs -> Either Error BondedPoolParams
fromBondedPoolArgs bpa = do
  iterations <- fromSdkNat' "iterations" bpa.iterations
  interest <- fromSdkInterest context bpa.interest
  minStake <- fromSdkNat' "minStake" bpa.minStake
  maxStake <- fromSdkNat' "maxStake" bpa.maxStake
  admin <- fromSdkAdmin context bpa.admin
  bondedAssetClass <- fromSdkAssetClass context bpa.bondedAssetClass
  nftCs <- fromSdkCurrencySymbol context bpa.nftCs
  assocListCs <- fromSdkCurrencySymbol context bpa.assocListCs
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
    , bondedAssetClass
    , nftCs
    , assocListCs
    }
  where
  fromSdkNat' :: String -> BigInt -> Either Error Natural
  fromSdkNat' name = fromSdkNat context name

  context :: String
  context = "fromBondedPoolArgs"

--Unbonded----------------------------------------------------------------------

type UnbondedPoolArgs =
  { start :: BigInt -- like POSIXTime
  , userLength :: BigInt -- like POSIXTime
  , adminLength :: BigInt -- like POSIXTime
  , bondingLength :: BigInt -- like POSIXTime
  , interestLength :: BigInt -- like POSIXTime
  , increments :: BigInt -- Natural
  , interest :: Tuple BigInt BigInt -- Rational
  , minStake :: BigInt -- Natural
  , maxStake :: BigInt -- Natural
  , admin :: String -- PaymentPubKeyHash
  , unbondedAssetClass ::
      Tuple String String -- AssetClass ~ Tuple CBORCurrencySymbol ASCIITokenName
  , nftCs :: String -- CurrencySymbol
  , assocListCs :: String -- CurrencySymbol
  }

type InitialUnbondedArgs =
  { start :: BigInt -- like POSIXTime
  , userLength :: BigInt -- like POSIXTime
  , adminLength :: BigInt -- like POSIXTime
  , interestLength :: BigInt -- like POSIXTime
  , bondingLength :: BigInt -- like POSIXTime
  , increments :: BigInt -- Natural
  , interest :: Tuple BigInt BigInt -- Rational
  , minStake :: BigInt -- Natural
  , maxStake :: BigInt -- Natural
  , unbondedAssetClass ::
      Tuple String String -- AssetClass ~ Tuple CBORCurrencySymbol ASCIITokenName
  }

callCreateUnbondedPool
  :: ContractConfig ()
  -> InitialUnbondedArgs
  -> Effect (Promise UnbondedPoolArgs)
callCreateUnbondedPool cfg iba = Promise.fromAff do
  iup <- liftEither $ fromInitialUnbondedArgs iba
  upp <- runContract cfg $ createUnbondedPoolContract iup
  pure $ toUnbondedPoolArgs upp

callDepositUnbondedPool
  :: ContractConfig ()
  -> UnbondedPoolArgs
  -> BigInt
  -> Array Int
  -> Effect (Promise (Array Int))
callDepositUnbondedPool cfg upa bi arr = Promise.fromAff $ runContract cfg do
  upp <- liftEither $ fromUnbondedPoolArgs upa
  nat <- liftM (error "callDepositUnbondedPool: Invalid natural number")
    $ fromBigInt bi
  depositUnbondedPoolContract upp nat arr

callCloseUnbondedPool
  :: ContractConfig ()
  -> UnbondedPoolArgs
  -> BigInt
  -> Array Int
  -> Effect (Promise (Array Int))
callCloseUnbondedPool cfg upa bi arr = Promise.fromAff $ runContract cfg do
  upp <- liftEither $ fromUnbondedPoolArgs upa
  nat <- liftM (error "callCloseUnbondedPool: Invalid natural number")
    $ fromBigInt bi
  closeUnbondedPoolContract upp nat arr

callUserStakeUnbondedPool
  :: ContractConfig () -> UnbondedPoolArgs -> BigInt -> Effect (Promise Unit)
callUserStakeUnbondedPool cfg upa bi = Promise.fromAff $ runContract cfg do
  upp <- liftEither $ fromUnbondedPoolArgs upa
  nat <- liftM (error "callUserStakeUnbondedPool: Invalid natural number")
    $ fromBigInt bi
  userStakeUnbondedPoolContract upp nat

callUserWithdrawUnbondedPool
  :: ContractConfig () -> UnbondedPoolArgs -> Effect (Promise Unit)
callUserWithdrawUnbondedPool =
  callWithUnbondedPoolArgs userWithdrawUnbondedPoolContract

callWithUnbondedPoolArgs
  :: (UnbondedPoolParams -> Contract () Unit)
  -> ContractConfig ()
  -> UnbondedPoolArgs
  -> Effect (Promise Unit)
callWithUnbondedPoolArgs contract cfg = callWithArgs fromUnbondedPoolArgs
  contract
  cfg

toUnbondedPoolArgs :: UnbondedPoolParams -> UnbondedPoolArgs
toUnbondedPoolArgs (UnbondedPoolParams upp) =
  { start: upp.start
  , userLength: upp.userLength
  , adminLength: upp.adminLength
  , bondingLength: upp.bondingLength
  , interestLength: upp.interestLength
  , increments: toBigInt upp.increments
  , interest: toSdkInterest upp.interest
  , minStake: toBigInt upp.minStake
  , maxStake: toBigInt upp.maxStake
  , admin: toSdkAdmin upp.admin
  , unbondedAssetClass: toSdkAssetClass upp.unbondedAssetClass
  , nftCs: toSdkCurrencySymbol upp.nftCs
  , assocListCs: toSdkCurrencySymbol upp.assocListCs
  }

fromUnbondedPoolArgs :: UnbondedPoolArgs -> Either Error UnbondedPoolParams
fromUnbondedPoolArgs upa = do
  interest <- fromSdkInterest context upa.interest
  minStake <- fromSdkNat' "minStake" upa.minStake
  maxStake <- fromSdkNat' "maxStake" upa.maxStake
  increments <- fromSdkNat' "increments" upa.increments
  admin <- fromSdkAdmin context upa.admin
  unbondedAssetClass <- fromSdkAssetClass context upa.unbondedAssetClass
  nftCs <- fromSdkCurrencySymbol context upa.nftCs
  assocListCs <- fromSdkCurrencySymbol context upa.assocListCs
  pure $ wrap
    { start: upa.start
    , userLength: upa.userLength
    , adminLength: upa.adminLength
    , bondingLength: upa.bondingLength
    , interestLength: upa.interestLength
    , increments
    , interest
    , minStake
    , maxStake
    , admin
    , unbondedAssetClass
    , nftCs
    , assocListCs
    }
  where
  fromSdkNat' :: String -> BigInt -> Either Error Natural
  fromSdkNat' name = fromSdkNat context name

  context :: String
  context = "fromUnbondedPoolArgs"

fromInitialUnbondedArgs
  :: InitialUnbondedArgs -> Either Error InitialUnbondedParams
fromInitialUnbondedArgs iba = do
  interest <- fromSdkInterest context iba.interest
  increments <- fromSdkNat' "increments" iba.increments
  minStake <- fromSdkNat' "minStake" iba.minStake
  maxStake <- fromSdkNat' "maxStake" iba.maxStake
  unbondedAssetClass <- fromSdkAssetClass context iba.unbondedAssetClass
  pure $ wrap
    { start: iba.start
    , userLength: iba.userLength
    , adminLength: iba.adminLength
    , bondingLength: iba.bondingLength
    , interestLength: iba.interestLength
    , interest
    , increments
    , minStake
    , maxStake
    , unbondedAssetClass
    }
  where
  fromSdkNat' :: String -> BigInt -> Either Error Natural
  fromSdkNat' name = fromSdkNat context name

  context :: String
  context = "fromInitialUnbondedArgs"
