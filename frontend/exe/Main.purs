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
  , mkContractConfig
  , runContract_
  )
import Contract.Wallet (mkNamiWalletAff)
import CreatePool (createPoolContract)
import DepositPool (depositPoolContract)
import Effect.Aff (launchAff_)

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
  runContract_ cfg createPoolContract
--runContract_ cfg depositPoolContract
