module Scripts.StateNFT
  ( mkStateNFTPolicy
  ) where

import Contract.Prelude

import Data.Argonaut (Json, JsonDecodeError)
import ToData (toData)
import Contract.Monad (Contract, liftedE)
import Contract.Scripts
  ( MintingPolicy(..)
  , applyArgs
  )
import QueryM (ClientError)
import Types.UnbalancedTransaction (TxOutRef)
import Types.Scripts (PlutusScript)
import Types (StakingType(Bonded, Unbonded))
import Utils (jsonReader)

-- | This is the parameterized minting policy. It still needs to receive a
-- `TxOutRef` to become a minting policy
nftPolicy :: StakingType -> Either JsonDecodeError PlutusScript
nftPolicy Bonded = jsonReader "script" _bondedStateNFT
nftPolicy Unbonded = jsonReader "script" _unbondedStateNFT

-- | This function takes a `TxOutRef` and produces the `MintingPolicy` for
-- the state NFT
mkStateNFTPolicy
  :: forall (r :: Row Type) (a :: Type)
   . StakingType
  -> TxOutRef
  -> Contract r (Either ClientError MintingPolicy)
mkStateNFTPolicy st txOutRef = do
  unappliedScript <- liftedE $ pure $ nftPolicy st
  applyArgs (MintingPolicy unappliedScript) [ toData txOutRef ]

foreign import _bondedStateNFT :: Json
foreign import _unbondedStateNFT :: Json
