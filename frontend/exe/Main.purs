module Main (main) where

import Contract.Prelude

-- import ClosePool (closeBondedPoolContract)
import Contract.Address (NetworkId(TestnetId))
import Contract.Monad
  ( ContractConfig
  , ConfigParams(ConfigParams)
  , LogLevel(Info)
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , launchAff_
  , liftContractM
  , logInfo'
  , mkContractConfig
  , runContract
  , runContract_
  )
import Contract.Numeric.Natural (fromString) as Natural
import Contract.Wallet (mkNamiWalletAff)
import CreatePool (createBondedPoolContract)
import Data.Int (toNumber)
-- import DepositPool (depositBondedPoolContract)
import Effect.Aff (delay)
import Effect.Exception (error)
import Settings (testInitBondedParams)
import UserStake (userStakeBondedPoolContract)

-- import Settings (testInitUnbondedParams)
-- import UnbondedStaking.CreatePool (createUnbondedPoolContract)
-- import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)

-- import Settings (testInitUnbondedParams)
-- import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
-- import UnbondedStaking.CreatePool (createUnbondedPoolContract)
-- import UnbondedStaking.DepositPool (depositUnbondedPoolContract)

-- main :: Effect Unit
-- main = launchAff_ $ do
--   cfg <- mkConfig
--   runContract_ cfg $ do
--     initParams <- liftContractM "main: Cannot initiate bonded parameters"
--       testInitBondedParams
--     bondedParams <- createBondedPoolContract initParams
--     -- sleep in order to wait for tx
--     liftAff $ delay $ wrap $ toNumber 80_000
--     depositBondedPoolContract bondedParams
--     liftAff $ delay $ wrap $ toNumber 80_000
--     closeBondedPoolContract bondedParams

-- -- Unbonded test
-- initParams <- liftContractM "main: Cannot initiate unbonded parameters"
--   testInitUnbondedParams
-- unbondedParams <- createUnbondedPoolContract initParams
-- -- sleep in order to wait for tx
-- liftAff $ delay $ wrap $ toNumber 80_000
-- depositUnbondedPoolContract unbondedParams
-- liftAff $ delay $ wrap $ toNumber 80_000
-- closeUnbondedPoolContract unbondedParams

main :: Effect Unit
main = launchAff_ do
  adminCfg <- mkConfig
  bondedParams <-
    runContract adminCfg do
      initParams <- liftContractM "main: Cannot initiate bonded parameters"
        testInitBondedParams
      bondedParams <- createBondedPoolContract initParams
      logInfo' "SWITCH WALLETS NOW TO CHANGE USERS"
      liftAff $ delay $ wrap $ toNumber 80_000
      pure bondedParams
  userCfg <- mkConfig
  userStake <- liftM (error "Cannot create Natural") $ Natural.fromString "5"
  runContract_ userCfg $ userStakeBondedPoolContract bondedParams userStake

-- Unbonded Test
-- main :: Effect Unit
-- main = launchAff_ do
--   adminCfg <- mkConfig
--   unbondedParams <-
--     runContract adminCfg do
--       initParams <- liftContractM "main: Cannot initiate unbonded parameters"
--         testInitUnbondedParams
--       unbondedParams <- createUnbondedPoolContract initParams
--       logInfo' "SWITCH WALLETS NOW TO CHANGE USERS"
--       liftAff $ delay $ wrap $ toNumber 80_000
--       pure unbondedParams
--   userCfg <- mkConfig
--   userStake <- liftM (error "Cannot create Natural") $ Natural.fromString "5000000"
--   runContract_ userCfg $ userStakeUnbondedPoolContract unbondedParams userStake

mkConfig :: Aff (ContractConfig ())
mkConfig = do
  wallet <- Just <$> mkNamiWalletAff
  mkContractConfig $ ConfigParams
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: defaultServerConfig
    , networkId: TestnetId
    , logLevel: Info
    , extraConfig: {}
    , wallet
    }
