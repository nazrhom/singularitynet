module SingularityNet.Validator
  ( bondedValidator
  ) where

import Contract.Prelude
import Contract.PlutusData (PlutusData)
import Contract.Scripts (Validator)
import Data.Argonaut (Json, JsonDecodeError)
import SingularityNet.Helpers (jsonReader)

-- This is the fully applied validator (no validator parameters) and should be
-- done on Haskell side until we have `applyCode`
bondedValidator :: Either JsonDecodeError Validator
bondedValidator = jsonReader "validator" _bondedValidator

foreign import _bondedValidator :: Json