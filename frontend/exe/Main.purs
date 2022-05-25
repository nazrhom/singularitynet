module Main (main) where

import Contract.Prelude

import ClosePool (closePoolContract)
import Contract.Address (NetworkId(TestnetId))
import Contract.Monad
  ( ConfigParams(ConfigParams)
  , LogLevel(Info)
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , defaultSlotConfig
  , launchAff_
  , liftContractM
  , mkContractConfig
  , runContract_
  )
import Contract.Wallet (mkNamiWalletAff)
import CreatePool (createBondedPoolContract)
import Data.Int (toNumber)
import DepositPool (depositPoolContract)
import Effect.Aff (delay)
import Settings (testInitBondedParams)

-- import Settings (testInitUnbondedParams)
-- import UnbondedStaking.CreatePool (createUnbondedPoolContract)

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
    initParams <- liftContractM "main: Cannot initiate bonded parameters"
      testInitBondedParams
    bondedParams <- createBondedPoolContract initParams
    -- sleep in order to wait for tx
    liftAff $ delay $ wrap $ toNumber 80_000
    depositPoolContract bondedParams
    liftAff $ delay $ wrap $ toNumber 80_000
    closePoolContract bondedParams

-- Unbonded test
-- initParams <- liftContractM "main: Cannot initiate unbonded parameters"
--   testInitUnbondedParams
-- void $ createUnbondedPoolContract initParams
