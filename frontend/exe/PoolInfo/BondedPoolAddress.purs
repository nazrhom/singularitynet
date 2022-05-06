module PoolInfo.BondedPoolAddress(
    bondedPoolAddress
) where

import Contract.Prelude

import Serialization.Address(Address, baseAddressToAddress, baseAddressFromBech32)
import Contract.Monad (Contract, liftContractE)
import Data.Argonaut (Json, JsonDecodeError)
import Types.Aliases (Bech32String)
import Utils (jsonReader)

addressAsBech32' :: Either JsonDecodeError Bech32String
addressAsBech32' = jsonReader "address" _bondedPoolAddress

bondedPoolAddress ::
    forall (r :: Row Type) (a :: Type). Contract r (Maybe Address)
bondedPoolAddress = do
    addressAsBech32 <- liftContractE addressAsBech32'
    res <- pure $ baseAddressToAddress <$> baseAddressFromBech32 addressAsBech32
    pure res
    
foreign import _bondedPoolAddress :: Json