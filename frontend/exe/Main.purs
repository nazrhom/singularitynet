module Main (main) where

import Contract.Prelude

import BondedStaking.TimeUtils (startPoolFromNow)
import ClosePool (closeBondedPoolContract)
import Contract.Address (NetworkId(TestnetId))
import Contract.Monad (ContractConfig, ConfigParams(ConfigParams), LogLevel(Info), defaultDatumCacheWsConfig, defaultOgmiosWsConfig, defaultServerConfig, launchAff_, liftContractM, logInfo', mkContractConfig, runContract, runContract_)
import Contract.Numeric.Natural (fromBigInt, fromString, toBigInt)
import Contract.Numeric.Natural (fromString) as Natural
import Contract.Wallet (mkNamiWalletAff)
import CreatePool (createBondedPoolContract)
import Data.BigInt (fromInt)
import Data.Int (toNumber)
import DepositPool (depositBondedPoolContract)
import Effect.Aff (delay)
import Effect.Exception (error)
import Settings (testInitBondedParams)
import Types (InitialBondedParams(..))
import Types.Interval (POSIXTime(..))
import Types.Natural (toBigInt)
import UserStake (userStakeBondedPoolContract)
import Utils (big, currentRoundedTime, logInfo_)

-- import Settings (testInitUnbondedParams)
-- import UnbondedStaking.CloseUnbondedPool (closeUnbondedPoolContract)
-- import UnbondedStaking.CreateUnbondedPool (createUnbondedPoolContract)
-- import UnbondedStaking.DepositUnbondedPool (depositUnbondedPoolContract)

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
      -- We get the current time and set up the pool to start 80 seconds from now
      let startDelayInt :: Int
          startDelayInt = 80_000
      startDelay <- liftContractM "main: Cannot create startDelay from Int" $
        fromBigInt $ fromInt startDelayInt
      initParams' /\ currTime <- startPoolFromNow startDelay initParams
      logInfo_ "Pool creation time" currTime
      bondedParams <- createBondedPoolContract initParams'
      logInfo_ "Pool parameters" bondedParams
      logInfo' "SWITCH WALLETS NOW - CHANGE TO USER 1"
      -- We give 30 seconds of margin for the user and admin to sign the transactions
      liftAff $ delay $ wrap $ toNumber $ startDelayInt + 30_000
      pure bondedParams
  -- User 1 deposits
  userCfg <- mkConfig
  userStake <- liftM (error "main: Cannot create userStake from String") $ Natural.fromString "10"
  runContract_ userCfg do
    userStakeBondedPoolContract bondedParams userStake
    logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
    -- Wait until bonding period
    liftAff $ delay $ wrap $ toNumber $ 180_000
  -- Admin deposits to pool
  runContract_ adminCfg do
    depositBondedPoolContract bondedParams
    logInfo' "END"
    --logInfo' "DON'T SWITCH WALLETS - STAY AS ADMIN"
    --liftAff $ delay $ wrap $ toNumber 100_000
  -- Admin closes pool
  -- runContract_ adminCfg $ closeBondedPoolContract bondedParams

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