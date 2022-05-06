module Scripts.BondedStateNFT
  ( mkBondedStateNFTPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.Scripts (MintingPolicy(MintingPolicy), applyArgs)
import Data.Argonaut (Json, JsonDecodeError)
import QueryM (ClientError)
import ToData (toData)
import Types.Scripts (PlutusScript)
import Types.UnbalancedTransaction (TxOutRef)
import Utils (jsonReader)

-- | This is the parameterized minting policy. It still needs to receive a
-- `TxOutRef` to become a minting policy
bondedStateNFTPolicy :: Either JsonDecodeError PlutusScript
bondedStateNFTPolicy = jsonReader "script" _bondedStateNFT

-- | This function takes a `TxOutRef` and produces the `MintingPolicy` for
-- the state NFT
mkBondedStateNFTPolicy
  :: forall (r :: Row Type) (a :: Type)
   . TxOutRef
  -> Contract r (Either ClientError MintingPolicy)
mkBondedStateNFTPolicy txOutRef = do
  unappliedScript <- liftedE $ pure $ bondedStateNFTPolicy
  let txOutRef' = unwrap txOutRef
  applyArgs (MintingPolicy unappliedScript)
    [ toData $ txOutRef'.transactionId
    , toData txOutRef'.index
    ]

foreign import _bondedStateNFT :: Json
