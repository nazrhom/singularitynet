module Main (main) where

import Contract.Prelude

import Contract.Address (NetworkId(TestnetId))
import Contract.Monad
  ( ConfigParams(ConfigParams)
  , LogLevel(Info)
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , defaultSlotConfig
  , launchAff_
  , mkContractConfig
  , runContract_
  )
import Contract.Wallet (mkNamiWalletAff)
import CreatePool (createPoolContract)
import Data.Int (toNumber)
import DepositPool (depositPoolContract)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  cfg <- mkContractConfig $ ConfigParams
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: defaultServerConfig
    , networkId: TestnetId
    , slotConfig: defaultSlotConfig
    , logLevel: Info
    , extraConfig: {}
    , wallet
    }
  runContract_ cfg $ do
    poolInfo <- createPoolContract
    -- sleep in order to wait for tx
    liftAff $ delay $ wrap $ toNumber 10000
    depositPoolContract poolInfo
