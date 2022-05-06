module PoolInfo.ListNFTSymbol(
    listNFTSymbol
) where

import Data.Argonaut (Json, JsonDecodeError)
import Contract.Prelude
import Contract.Monad (Contract, liftContractE)
import Contract.Value (CurrencySymbol)
import Types.ByteArray (ByteArray)
import Plutus.Types.CurrencySymbol(mkCurrencySymbol)
import Utils (jsonReader)

symbolByteArray' :: Either JsonDecodeError ByteArray
symbolByteArray' = jsonReader "unCurrencySymbol" _listNFTSymbol

listNFTSymbol ::
    forall (r :: Row Type) (a :: Type). Contract r (Maybe CurrencySymbol)
listNFTSymbol = do
    symbolByteArray <- liftContractE symbolByteArray'
    pure $ mkCurrencySymbol symbolByteArray
    
foreign import _listNFTSymbol :: Json