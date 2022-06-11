module Types(
    InitialBondedPoolParams(..)
    , BondedPoolScripts(..),
    BondedPoolTypedScripts(..)
    , TBondedPool(..)
    , TEntry(..)
    , TAsset(..)
    ) where
import SingularityNet.Natural (Natural, NatRatio)
import Plutus.V1.Ledger.Api (POSIXTime, Validator, MintingPolicy, Script, TxOutRef)
import SingularityNet.Types (AssetClass, BondedStakingDatum)

-- | Bonded pool configuration without the fields that depend on the
-- | initial UTxO
data InitialBondedPoolParams = InitialBondedPoolParams
  { initIterations :: Natural
  , initStart :: POSIXTime   
  , initEnd :: POSIXTime
  , initUserLength :: POSIXTime
  , initBondingLength :: POSIXTime
  , initInterest :: NatRatio
  , initMinStake :: Natural
  , initMaxStake :: Natural
  , initBondedAssetClass :: AssetClass
  } deriving stock Show
  
-- | Datatype that contains all the scripts used in the bonded pool
data BondedPoolTypedScripts = BondedPoolTypedScripts {
    validator :: Validator,
    statePolicy :: MintingPolicy,
    listPolicy :: MintingPolicy
} deriving stock Show

-- | Datatype that contains all the typed scripts used in the bonded pool
data BondedPoolScripts = BondedPoolScripts {
    validatorScript :: Script,
    statePolicyScript :: Script,
    listPolicyScript :: Script
}

-- | Datatype that contains all the needed information to consume the pool UTxO
-- | Useful to avoid fetching/parsing datums and filtering UTxO in tests
data TBondedPool = TBondedPool {
    tbpTxOutRef :: TxOutRef
    , tbpStateDatum :: BondedStakingDatum
}

-- | Datatype that contains all the needed information to consume an entry
-- | Useful to avoid fetching/parsing datums and filtering UTxO in tests
data TEntry = TEntry {
    teTxOutRef :: TxOutRef
    , teEntryDatum :: BondedStakingDatum
}

-- | Datatype that contains all the needed information to consume an asset
-- | output. | Useful to avoid fetching/parsing datums and filtering UTxO in
-- | tests
data TAsset = TAsset {
    taTxOutRef :: TxOutRef
}