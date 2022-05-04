module Scripts.BondedPoolValidator(
    mkBondedPoolValidator
) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.Scripts (MintingPolicy(..), Validator(..), applyArgs)
import Data.Argonaut (Json, JsonDecodeError)
import QueryM (ClientError)
import ToData (toData)
import Types (BondedPoolParams(..))
import Types.Scripts (PlutusScript)
import Types.Value (CurrencySymbol)
import Utils (jsonReader)

-- | This is the parameterized validator script. It still needs to receive a
-- `BondedPoolParams` to become a minting policy
bondedPoolValidator :: Either JsonDecodeError PlutusScript
bondedPoolValidator = jsonReader "script" _bondedPoolValidator

-- | This function takes a `BondedPoolParams` and produces the `Validator`
-- for the bonded pool
mkBondedPoolValidator ::
    forall (r :: Row Type) .
    BondedPoolParams -> Contract r (Either ClientError Validator)
mkBondedPoolValidator params = do
    unappliedScript <- liftedE $ pure $ bondedPoolValidator
    applyArgs (Validator unappliedScript) [ toData params ]
    
foreign import _bondedPoolValidator :: Json