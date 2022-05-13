module Scripts.PoolValidator
  ( mkBondedPoolValidator
  , mkUnbondedPoolValidator
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.Scripts (Validator(Validator), applyArgs)
import Data.Argonaut (Json, JsonDecodeError)
import QueryM (ClientError)
import ToData (toData)
import Types (BondedPoolParams)
import UnbondedStaking.Types (UnbondedPoolParams)
import Types.Scripts (PlutusScript)
import Utils (jsonReader)

-- | This is the parameterized validator script. It still needs to receive a
-- `BondedPoolParams` to become a minting policy
bondedPoolValidator :: Either JsonDecodeError PlutusScript
bondedPoolValidator = jsonReader "script" _bondedPoolValidator

unbondedPoolValidator :: Either JsonDecodeError PlutusScript
unbondedPoolValidator = jsonReader "script" _unbondedPoolValidator

-- | This function takes a `BondedPoolParams` and produces the `Validator`
-- for the bonded pool
mkBondedPoolValidator
  :: forall (r :: Row Type)
   . BondedPoolParams
  -> Contract r (Either ClientError Validator)
mkBondedPoolValidator params = do
  unappliedScript <- liftedE $ pure $ bondedPoolValidator
  applyArgs (Validator unappliedScript) [ toData params ]

-- | This function takes a `UnbondedPoolParams` and produces the `Validator`
-- for the bonded pool
mkUnbondedPoolValidator
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> Contract r (Either ClientError Validator)
mkUnbondedPoolValidator params = do
  unappliedScript <- liftedE $ pure $ unbondedPoolValidator
  applyArgs (Validator unappliedScript) [ toData params ]

foreign import _bondedPoolValidator :: Json
foreign import _unbondedPoolValidator :: Json
