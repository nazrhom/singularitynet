module Scripts.BondedListNFT(
    mkBondedListNFTPolicy
) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Contract.Scripts (MintingPolicy(..), applyArgs)
import Contract.Value (CurrencySymbol)
import Data.Argonaut (Json, JsonDecodeError)
import QueryM (ClientError)
import ToData (toData)
import Types.Scripts (PlutusScript)
import Utils (jsonReader)

-- | This is the parameterized minting policy. It still needs to receive a
-- `CurrencySymbol` to become a minting policy
bondedListNFTPolicy :: Either JsonDecodeError PlutusScript
bondedListNFTPolicy = jsonReader "script" _bondedListNFT

-- | This function takes a `CurrencySymbol` and produces the `MintingPolicy` for
-- the list NFT
mkBondedListNFTPolicy ::
    forall (r :: Row Type) (a :: Type).
    CurrencySymbol -> Contract r (Either ClientError MintingPolicy)
mkBondedListNFTPolicy nftCs = do
    unappliedScript <- liftContractE $ bondedListNFTPolicy
    applyArgs (MintingPolicy unappliedScript) [ toData nftCs ]
    
foreign import _bondedListNFT :: Json