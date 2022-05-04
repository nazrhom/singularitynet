module Scripts.BondedStateNFT(
    mkBondedStateNFTPolicy
) where

import Contract.Prelude

import Data.Argonaut (Json, JsonDecodeError)
import ToData(toData)
import Contract.Monad(Contract, liftedE)
import Contract.Scripts (
    MintingPolicy(..)
    , applyArgs
    )
import QueryM(ClientError)
import Types.UnbalancedTransaction (TxOutRef)
import Types.Scripts(PlutusScript)
import Utils (jsonReader)

-- | This is the parameterized minting policy. It still needs to receive a
-- `TxOutRef` to become a minting policy
bondedStateNFTPolicy :: Either JsonDecodeError PlutusScript
bondedStateNFTPolicy = jsonReader "script" _bondedStateNFT

-- | This function takes a `TxOutRef` and produces the `MintingPolicy` for
-- the state NFT
mkBondedStateNFTPolicy ::
    forall (r :: Row Type) (a :: Type).
    TxOutRef -> Contract r (Either ClientError MintingPolicy)
mkBondedStateNFTPolicy txOutRef = do
    unappliedScript <- liftedE $ pure $ bondedStateNFTPolicy
    applyArgs (MintingPolicy unappliedScript) [ toData txOutRef ]
    
foreign import _bondedStateNFT :: Json
