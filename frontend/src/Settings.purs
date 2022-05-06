module Settings
  ( bondedStakingTokenName
  , hardCodedParams
  ) where

import Prelude

import Contract.Numeric.NatRatio (fromNaturals, toRational)
import Contract.Numeric.Rational (Rational)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , adaSymbol
  , adaToken
  , mkTokenName
  )
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Types (AssetClass(AssetClass), BondedPoolParams(BondedPoolParams))
import Types.ByteArray (byteArrayFromAscii)
import Types.UnbalancedTransaction (PaymentPubKeyHash)
import Utils (nat, big)

bondedStakingTokenName :: Maybe TokenName
bondedStakingTokenName = mkTokenName =<< byteArrayFromAscii "BondedStakingToken"

interest' :: Maybe Rational
interest' = toRational <$> fromNaturals (nat 1) (nat 100)

-- Temporary, serialise to JSON
hardCodedParams
  :: PaymentPubKeyHash
  -> CurrencySymbol
  -> CurrencySymbol
  -> Maybe BondedPoolParams
hardCodedParams adminPkh nftCs assocListCs = do
  interest <- interest'
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
    , bondedAssetClass: AssetClass $ adaSymbol /\ adaToken
    , nftCs
    , assocListCs
    }
