module UnbondedStaking.AdminUtils
  ( calculateRewards
  -- , mkEntryUpdateList
  , submitTransaction
  ) where

import Contract.Prelude

import Contract.Monad
  ( Contract
  , liftedE
  , liftedM
  , throwContractError
  )
import Contract.Numeric.Rational (Rational, (%))
import Contract.PlutusData (PlutusData)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Control.Monad.Error.Class (try)
import Data.Array as Array
import Data.BigInt (BigInt)
import Utils (logInfo_, mkRatUnsafe)

-- | Submits a transaction with the given list of constraints/lookups
submitTransaction
  :: TxConstraints Unit Unit
  -> ScriptLookups.ScriptLookups PlutusData
  -> Array
       ( Tuple
           (TxConstraints Unit Unit)
           (ScriptLookups.ScriptLookups PlutusData)
       )
  -> Contract ()
       ( Array
           ( Tuple
               (TxConstraints Unit Unit)
               (ScriptLookups.ScriptLookups PlutusData)
           )
       )
submitTransaction baseConstraints baseLookups updateList = do
  let
    constraintList = fst <$> updateList
    lookupList = snd <$> updateList
    constraints = baseConstraints <> mconcat constraintList
    lookups = baseLookups <> mconcat lookupList
  result <- try do
    -- Build transaction
    unattachedBalancedTx <-
      liftedE $ ScriptLookups.mkUnbalancedTx lookups constraints
    logInfo_
      "submitTransaction: unAttachedUnbalancedTx"
      unattachedBalancedTx
    BalancedSignedTransaction { signedTxCbor } <-
      liftedM
        "submitTransaction: Cannot balance, reindex redeemers, /\
        \attach datums redeemers and sign"
        $ balanceAndSignTx unattachedBalancedTx
    -- Submit transaction using Cbor-hex encoded `ByteArray`
    transactionHash <- submit signedTxCbor
    logInfo_
      "submitTransaction: Transaction successfully submitted with /\
      \hash"
      $ byteArrayToHex
      $ unwrap transactionHash
  case result of
    Left e -> do
      logInfo_ "depositUnbondedPoolContract:" e
      pure $ Array.singleton $ constraints /\ lookups
    Right _ ->
      pure []

-- | Calculates user awards according to spec formula
calculateRewards
  :: Rational
  -> BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> Contract () Rational
calculateRewards rewards totalRewards deposited newDeposit totalDeposited = do
  when (totalDeposited == zero) $
    throwContractError "calculateRewards: totalDeposited is zero"
  let
    lhs = mkRatUnsafe $ totalRewards % totalDeposited
    rhs = rewards + mkRatUnsafe (deposited % one)
    rhs' = rhs - mkRatUnsafe (newDeposit % one)
    f = rhs' * lhs
  when (f < zero) $ throwContractError
    "calculateRewards: invalid rewards amount"
  pure $ rewards + f
