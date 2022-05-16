module Settings
  ( agixCs
  , agixTn
  , bondedStakingTokenName
  , ntxCs
  , ntxTn
  , testInitBondedParams
  ) where

import Prelude

import Contract.Numeric.NatRatio (fromNaturals, toRational)
import Contract.Numeric.Rational (Rational)
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArray)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , mkCurrencySymbol
  , mkTokenName
  )
import Data.Maybe (Maybe)
import Types
  ( AssetClass(AssetClass)
  , InitialBondedParams(InitialBondedParams)
  )
import Utils (nat, big)

bondedStakingTokenName :: Maybe TokenName
bondedStakingTokenName = mkTokenName =<< byteArrayFromAscii "BondedStakingToken"

interest' :: Maybe Rational
interest' = toRational <$> fromNaturals (nat 1) (nat 100)

-- Could make these unsafe/partial for convenience:
agixCs :: Maybe CurrencySymbol
agixCs = mkCurrencySymbol
  =<< hexToByteArray "6f1a1f0c7ccf632cc9ff4b79687ed13ffe5b624cce288b364ebdce50"

agixTn :: Maybe TokenName
agixTn = mkTokenName =<< byteArrayFromAscii "AGIX"

ntxCs :: Maybe CurrencySymbol
ntxCs = mkCurrencySymbol
  =<< hexToByteArray "b5094f93ff9fcba9e8b257197d589cbcde3d92a108804e3a378bd2ce"

ntxTn :: Maybe TokenName
ntxTn = mkTokenName =<< byteArrayFromAscii "NTX"

-- Used for local example:
testInitBondedParams :: Maybe InitialBondedParams
testInitBondedParams = do
  interest <- interest'
  currencySymbol <- agixCs
  tokenName <- agixTn
  pure $ InitialBondedParams
    { iterations: nat 3
    , start: big 1000
    , end: big 2000
    , userLength: big 100
    , bondingLength: big 4
    , interest
    , minStake: nat 1000
    , maxStake: nat 10_000
    , bondedAssetClass: AssetClass
        { currencySymbol
        , tokenName
        }
    }
