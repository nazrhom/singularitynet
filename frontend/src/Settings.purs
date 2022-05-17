module Settings
  ( agixCs
  , agixTn
  , bondedStakingTokenName
  , bondedHardCodedParams
  , ntxCs
  , ntxTn
  , testInitUnbondedParams
  , unbondedStakingTokenName
  ) where

import Prelude

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
import Types (AssetClass(AssetClass), BondedPoolParams(BondedPoolParams))
import UnbondedStaking.Types (InitialUnbondedParams(InitialUnbondedParams))
import Types.UnbalancedTransaction (PaymentPubKeyHash)
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

unbondedStakingTokenName :: Maybe TokenName
unbondedStakingTokenName = mkTokenName =<< byteArrayFromAscii
  "UnbondedStakingToken"

-- Temporary, serialise to JSON
bondedHardCodedParams
  :: PaymentPubKeyHash
  -> CurrencySymbol
  -> CurrencySymbol
  -> Maybe BondedPoolParams
bondedHardCodedParams adminPkh nftCs assocListCs = do
  interest <- interest'
  currencySymbol <- agixCs
  tokenName <- agixTn
  pure $ BondedPoolParams
    { iterations: nat 3
    , start: big 1000
    , end: big 2000
    , userLength: big 100
    , bondingLength: big 4
    , interest
    , minStake: nat 1000
    , maxStake: nat 10_000
    , admin: adminPkh
    , bondedAssetClass: AssetClass
        { currencySymbol
        , tokenName
        }
    , nftCs
    , assocListCs
    }

testInitUnbondedParams :: Maybe InitialUnbondedParams
testInitUnbondedParams = do
  interest <- interest'
  -- currencySymbol <- agixCs
  -- tokenName <- ntxTn
  pure $ InitialUnbondedParams
    { start: big 1000
    , userLength: big 100
    , adminLength: big 100
    , bondingLength: big 4
    , interestLength: big 2
    , increments: nat 2
    , interest: interest
    , minStake: nat 1000
    , maxStake: nat 10_000
    , unbondedAssetClass: AssetClass
        { currencySymbol: adaSymbol
        , tokenName: adaToken
        }
    }
