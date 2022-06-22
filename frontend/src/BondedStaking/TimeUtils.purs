module BondedStaking.TimeUtils where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM, throwContractError)
import Contract.Numeric.Natural (toBigInt)
import Control.Alternative (guard)
import Data.Array (head)
import Data.BigInt (BigInt)
import Types (BondedPoolParams(BondedPoolParams))
import Types.Interval (POSIXTime(..), POSIXTimeRange, interval)
import Utils(big, bigIntRange, currentRoundedTime)

-- | Get the time-range that includes the current (approximate) time and the
-- | pool accepts as a valid staking period. If there is no such range,
-- | throw an error.
getStakingTime :: forall (r :: Row Type) .
  BondedPoolParams ->
  Contract r {currTime :: POSIXTime, range :: POSIXTimeRange}
getStakingTime (BondedPoolParams bpp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Throw error if staking is impossible
  when (currTime' > bpp.end) $
    throwContractError "getStakingTime: pool already closed"
  when (currTime' > bpp.end - bpp.userLength) $
    throwContractError "getStakingTime: pool in withdrawing only period"
  -- Get timerange in which the staking should be done
  let cycleLength :: BigInt
      cycleLength = bpp.bondingLength + bpp.userLength
      possibleRanges :: Array (BigInt /\ BigInt)
      possibleRanges = do
        -- Range from 0 to bpp.iterations
        n <- bigIntRange $ toBigInt bpp.iterations
        -- Calculate start and end of the range
        let range@(_start /\ end) = (bpp.start + n * cycleLength) /\ (bpp.start + n * cycleLength + bpp.userLength - big 1000)
        -- Discard range if end < currTime
        guard $ currTime' <= end
        -- NOTE: We don't discard these ranges yet because it's easier to debug when CTL submits
        -- the TX and fails loudly
        ---- Discard range if currTime < start
        --guard $ currTime' >= start
        pure range
  -- Return first range
  start /\ end <- liftContractM "getStakingTime: this is not a staking period" $
    head possibleRanges
  pure { currTime, range: interval (POSIXTime start) (POSIXTime end) }

-- | Get the time-range that includes the current (approximate) time and the
-- | pool accepts as a valid deposit period. If there is no such range,
-- | throw an error.
getBondingTime :: forall (r :: Row Type) .
  BondedPoolParams ->
  Contract r {currTime :: POSIXTime, range :: POSIXTimeRange}
getBondingTime (BondedPoolParams bpp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Throw error if depositing is impossible
  when (currTime' > bpp.end) $
    throwContractError "getBondingTime: pool already closed"
  when (currTime' > bpp.end - bpp.userLength) $
    throwContractError "getBondingTime: pool in withdrawing only period"
  -- Get timerange in which the staking should be done
  let cycleLength :: BigInt
      cycleLength = bpp.bondingLength + bpp.userLength
      possibleRanges :: Array (BigInt /\ BigInt)
      possibleRanges = do
        -- Range from 0 to bpp.iterations
        n <- bigIntRange $ toBigInt bpp.iterations
        -- Calculate start and end of the range
        let range@(_start /\ end) = (bpp.start + n * cycleLength + bpp.userLength) /\ (bpp.start + (n + one) * cycleLength - big 1000)
        -- Discard range if end < currTime
        guard $ currTime' <= end
        -- Discard range if currTime < start
        -- guard $ currTime' >= start
        pure range
  -- Return first range
  start /\ end <- liftContractM "getBondingTime: this is not a bonding period" $
    head possibleRanges
  pure { currTime, range: interval (POSIXTime start) (POSIXTime end) }

-- | Get the time-range that includes the current (approximate) time and the
-- | pool accepts as a withdrawing period. These periods are the same
-- | as the ones returned by `getStakingTime`, with the addition of a final
-- | withdrawing-only period before pool closure. If there is no such range,
-- | throw an error.
getWithdrawingTime :: forall (r :: Row Type) .
  BondedPoolParams ->
  Contract r {currTime :: POSIXTime, range :: POSIXTimeRange}
getWithdrawingTime (BondedPoolParams bpp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Throw error if depositing is impossible
  when (currTime' > bpp.end) $
    throwContractError "getBondingTime: pool already closed"
  -- If currTime is inside withdrawing-only period, return it. Otherwise,
  -- check all the staking periods
  if (currTime' > bpp.end - bpp.userLength)
    then pure { currTime, range: interval (POSIXTime $ bpp.end - bpp.userLength) (POSIXTime $ bpp.end - big 1000) }
    else do
        -- Get timerange in which the staking should be done
        let cycleLength :: BigInt
            cycleLength = bpp.bondingLength + bpp.userLength
            possibleRanges :: Array (BigInt /\ BigInt)
            possibleRanges = do
                -- Range from 0 to bpp.iterations
                n <- bigIntRange $ toBigInt bpp.iterations
                -- Calculate start and end of the range
                let range@(_start /\ end) = (bpp.start + n * cycleLength + bpp.userLength) /\ (bpp.start + (n + one) * cycleLength - big 1000)
                -- Discard range if end < currTime
                guard $ currTime' <= end
                -- Discard range if currTime < start
                -- guard $ currTime' >= start
                pure range
        -- Return first range
        start /\ end <- liftContractM "getBondingTime: this is not a bonding period" $
            head possibleRanges
        pure { currTime, range: interval (POSIXTime start) (POSIXTime end) }