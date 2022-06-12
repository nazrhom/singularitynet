module Settings
  ( agixCs
  , agixTn
  , bondedStakingTokenName
  , ntxCs
  , ntxTn
  , testInitBondedParams
  , testInitUnbondedParams
  , unbondedStakingTokenName
  ) where

import Contract.Prelude

import Contract.Numeric.NatRatio (fromNaturals, toRational)
import Contract.Numeric.Rational (Rational)
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArray)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , adaSymbol
  , adaToken
  , mkCurrencySymbol
  , mkTokenName
  )
import Data.Maybe (Maybe)
import Types
  ( AssetClass(AssetClass)
  , InitialBondedParams(InitialBondedParams)
  )
import UnbondedStaking.Types (InitialUnbondedParams(InitialUnbondedParams))
import Utils (nat, big)

bondedStakingTokenName :: Maybe TokenName
bondedStakingTokenName = mkTokenName =<< byteArrayFromAscii "BondedStakingToken"

unbondedStakingTokenName :: Maybe TokenName
unbondedStakingTokenName = mkTokenName =<< byteArrayFromAscii
  "UnbondedStakingToken"

-- Defined as fixed rate for one cycle in APY
bondedInterest :: Maybe Rational
bondedInterest = toRational <$> fromNaturals (nat 1) (nat 100)

-- Defined as fixed rate for one time increment in APY
unbondedInterest :: Maybe Rational
unbondedInterest = toRational <$> fromNaturals (nat 1) (nat 100)

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
  interest <- bondedInterest
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

testInitUnbondedParams :: Maybe InitialUnbondedParams
testInitUnbondedParams = do
  interest <- unbondedInterest
  -- currencySymbol <- agixTn
  -- tokenName <- ntxTn
  pure $ InitialUnbondedParams
    { start: big 1000
    , userLength: big 100
    , adminLength: big 100
    , bondingLength: big 100
    , interestLength: big 2
    , increments: nat 50
    , interest: interest
    , minStake: nat 1
    , maxStake: nat 100_000_000
    , unbondedAssetClass: AssetClass
        { currencySymbol: adaSymbol
        , tokenName: adaToken
        }
    }
