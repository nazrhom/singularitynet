module Main (main) where

import Contract.Prelude

-- import BondedCallContract
--   ( bondedCallContractAdminCloseExample1
--   , bondedCallContractAdminDepositExample1
--   , bondedCallContractCreatePoolExample1
--   , bondedCallContractUserStakeExample1
--   )
import ClosePool (closeBondedPoolContract)
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
import DepositPool (depositBondedPoolContract)
import Effect.Aff (delay)
import Effect.Exception (error)
import Settings (testInitBondedParams)
import UserStake (userStakeBondedPoolContract)

-- import Data.BigInt (fromNumber)
-- import Data.JSDate (getTime, now)
-- import Effect.Class (liftEffect)
-- import Settings (testInitUnbondedParams)
-- import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
-- import UnbondedStaking.CreatePool (createUnbondedPoolContract)
-- import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
-- import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
-- import Utils (logInfo_)

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

-- Bonded: admin create pool, user stake, admin deposit (rewards), admin close
-- using PureScript (non SDK)
main :: Effect Unit
main = launchAff_ do
  adminCfg <- mkConfig
  -- Admin create pool
  bondedParams <-
    runContract adminCfg do
      logInfo' "STARTING AS ADMIN"
      initParams <- liftContractM "main: Cannot initiate bonded parameters"
        testInitBondedParams
      bondedParams <- createBondedPoolContract initParams
      logInfo' "SWITCH WALLETS NOW - CHANGE TO USER 1"
      liftAff $ delay $ wrap $ toNumber 80_000
      pure bondedParams
  -- User 1 deposits
  userCfg <- mkConfig
  userStake <- liftM (error "Cannot create Natural") $ Natural.fromString "10"
  runContract_ userCfg do
    userStakeBondedPoolContract bondedParams userStake
    logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
    liftAff $ delay $ wrap $ toNumber 100_000
  -- Admin deposits to pool
  runContract_ adminCfg do
    depositBondedPoolContract bondedParams
    logInfo' "DON'T SWITCH WALLETS - STAY AS ADMIN"
    liftAff $ delay $ wrap $ toNumber 100_000
  -- Admin closes pool
  runContract_ adminCfg $ closeBondedPoolContract bondedParams

-- main :: Effect Unit
-- main = launchAff_ do
--   adminCfg <- mkConfig
--   currDate <- liftEffect now
--   let currTime = fromMaybe zero $ fromNumber $ getTime currDate
--   -- Admin create pool
--   unbondedParams <-
--     runContract adminCfg do
--       logInfo' "STARTING AS ADMIN"
--       initParams <- liftContractM "main: Cannot initiate unbonded parameters" $
--         testInitUnbondedParams currTime
--       logInfo_ "main: Pool creation start time" currTime
--       unbondedParams <- createUnbondedPoolContract initParams
--       logInfo' "SWITCH WALLETS NOW - CHANGE TO USER 1"
--       liftAff $ delay $ wrap $ toNumber 80_000
--       pure unbondedParams
--   userCfg <- mkConfig
--   userStake <-
--     liftM (error "Cannot create Natural") $ Natural.fromString "5000000"
--   batchSize <- liftM (error "Cannot create Natural") $ Natural.fromString "1"
--   -- User 1 deposits
--   runContract_ userCfg do
--     userStakeUnbondedPoolContract unbondedParams userStake
--     logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
--     liftAff $ delay $ wrap $ toNumber 100_000
--   -- -- User 2 deposits
--   -- runContract_ userCfg do
--   --   userStakeUnbondedPoolContract unbondedParams userStake
--   --   logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
--   --   liftAff $ delay $ wrap $ toNumber 100_000
--   -- Admin deposits to pool
--   runContract_ adminCfg do
--     failedDeposits <- depositUnbondedPoolContract unbondedParams batchSize []
--       (\failedDeposits' -> do
--         logInfo' "main: Waiting to submit next Tx batch. DON'T SWITCH WALLETS - STAY AS ADMIN"
--         liftAff $ delay $ wrap $ toNumber 100_000
--       )
--     logInfo' "main: Closing pool..."
--   -- Admin closes pool
--   runContract_ adminCfg do
--     failedDeposits <- closeUnbondedPoolContract unbondedParams batchSize []
--       (\failedDeposits' -> do
--         logInfo' "main: Waiting to submit next Tx batch. DON'T SWITCH WALLETS - STAY AS ADMIN"
--         liftAff $ delay $ wrap $ toNumber 100_000
--       )
--     logInfo' "main: Pool closed"

-- Bonded: admin create pool, user stake, admin deposit (rewards), admin close
-- using PureScript (SDK)
-- Run *one* at a time:
-- main :: Effect Unit
-- main =
-- bondedCallContractCreatePoolExample1
-- After running the above contract, update `testBondedPoolArgs` accordingly.
-- bondedCallContractUserStakeExample1
-- bondedCallContractAdminDepositExample1
-- bondedCallContractAdminCloseExample1

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