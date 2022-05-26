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
  , defaultSlotConfig
  , launchAff_
  -- , liftContractM
  , mkContractConfig
  , runContract_
  )
import Contract.Wallet (mkNamiWalletAff)
import CreatePool (createBondedPoolContract)
import Data.Int (toNumber)
import DepositPool (depositBondedPoolContract)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)

-- import Settings (testInitUnbondedParams)
-- import UnbondedStaking.CloseUnbondedPool (closeUnbondedPoolContract)
-- import UnbondedStaking.CreateUnbondedPool (createUnbondedPoolContract)
-- import UnbondedStaking.DepositUnbondedPool (depositUnbondedPoolContract)

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
    -- Bonded test
    poolInfo <- createBondedPoolContract
    -- sleep in order to wait for tx
    liftAff $ delay $ wrap $ toNumber 80_000
    depositBondedPoolContract poolInfo
    liftAff $ delay $ wrap $ toNumber 80_000
    closeBondedPoolContract poolInfo

-- -- Unbonded test
-- initParams <- liftContractM "main: Cannot initiate unbonded parameters"
--   testInitUnbondedParams
-- unbondedParams <- createUnbondedPoolContract initParams
-- -- sleep in order to wait for tx
-- liftAff $ delay $ wrap $ toNumber 80_000
-- depositUnbondedPoolContract unbondedParams
-- liftAff $ delay $ wrap $ toNumber 80_000
-- closeUnbondedPoolContract unbondedParams
