module Main (main) where

import Contract.Prelude

import ClosePool (closeBondedPoolContract)
import Contract.Address (NetworkId(TestnetId))
import Contract.Monad
  ( ConfigParams(ConfigParams)
  , LogLevel(Info)
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , launchAff_
  , liftContractM
  , mkContractConfig
  , runContract_
  )
import Contract.Wallet (mkNamiWalletAff)
import CreatePool (createBondedPoolContract)
import Data.Int (toNumber)
import DepositPool (depositBondedPoolContract)
import Effect.Aff (delay)
import Settings (testInitBondedParams)

-- import Settings (testInitUnbondedParams)
-- import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
-- import UnbondedStaking.CreatePool (createUnbondedPoolContract)
-- import UnbondedStaking.DepositPool (depositUnbondedPoolContract)

main :: Effect Unit
main = launchAff_ $ do
  wallet <- Just <$> mkNamiWalletAff
  cfg <- mkContractConfig $ ConfigParams
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: defaultServerConfig
    , networkId: TestnetId
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
    depositBondedPoolContract bondedParams
    liftAff $ delay $ wrap $ toNumber 80_000
    closeBondedPoolContract bondedParams

-- -- Unbonded test
-- initParams <- liftContractM "main: Cannot initiate unbonded parameters"
--   testInitUnbondedParams
-- unbondedParams <- createUnbondedPoolContract initParams
-- -- sleep in order to wait for tx
-- liftAff $ delay $ wrap $ toNumber 80_000
-- depositUnbondedPoolContract unbondedParams
-- liftAff $ delay $ wrap $ toNumber 80_000
-- closeUnbondedPoolContract unbondedParams
