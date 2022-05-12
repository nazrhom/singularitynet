module SingularityNetOffchain (
    loadPlutusScript
) where

import Data.Aeson(decodeFileStrict)
import Plutus.V1.Ledger.Api (Script)

-- | Deserialises Plutus scripts under `scripts` folder
loadPlutusScript ::
    String ->   -- | Filename without extension (e.g: "BondedListNFT")
    IO (Maybe Script)
loadPlutusScript name = decodeFileStrict $ "scripts/" <> name <> ".plutus"
