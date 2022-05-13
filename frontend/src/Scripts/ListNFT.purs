module Scripts.ListNFT
  ( mkListNFTPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.Scripts (MintingPolicy(MintingPolicy), applyArgs)
import Contract.Value (CurrencySymbol)
import Data.Argonaut (Json, JsonDecodeError)
import QueryM (ClientError)
import ToData (toData)
import Types.Scripts (PlutusScript)
import Types (StakingType(..))
import Utils (jsonReader)

-- | This is the parameterized minting policy. It still needs to receive a
-- `CurrencySymbol` to become a minting policy
listNFTPolicy :: StakingType -> Either JsonDecodeError PlutusScript
listNFTPolicy Bonded = jsonReader "script" _bondedListNFT
listNFTPolicy Unbonded = jsonReader "script" _unbondedListNFT

-- | This function takes a `CurrencySymbol` and produces the `MintingPolicy` for
-- the list NFT
mkListNFTPolicy
  :: forall (r :: Row Type) (a :: Type)
   . StakingType
  -> CurrencySymbol
  -> Contract r (Either ClientError MintingPolicy)
mkListNFTPolicy st nftCs = do
  unappliedScript <- liftContractE $ listNFTPolicy st
  applyArgs (MintingPolicy unappliedScript) [ toData nftCs ]

foreign import _bondedListNFT :: Json
foreign import _unbondedListNFT :: Json
