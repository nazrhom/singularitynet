{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module BondedPool (
  pbondedPoolValidator,
  hbondedPoolValidator,
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic)

import Plutarch.Api.V1 (
  PScriptContext
  , PTxInfo
  , PPubKeyHash
  , mkValidator)
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Api.V1.Time (
  PPOSIXTimeRange
  )
import Plutus.V1.Ledger.Api (
  Validator)

import Types (
  BondedPoolParams,
  PBondedPoolParams,
  PBondedStakingAction(
    PAdminAct
    , PStakeAct
    , PWithdrawAct
    , PCloseAct
  ),
  PBondedStakingDatum,
 )
import Utils(
  guardC
  , pletC
  , pletDataC
  , pnestedIf
  , (>:)
  )
import Data.Natural (
  PNatural
  )
import Data.Interval(
  pcontains
  , pintervalFrom
  , pperiodicContains
  , pintervalTo
  , pinterval
  , PPeriodicInterval(
      PPeriodicInterval
      , piBaseOffset
      , piPeriod
      , piStartOffset
      , piEndOffset
      , piMaxCycles)
  )

pbondedPoolValidator ::
  forall (s :: S).
  Term
    s
    ( PBondedPoolParams
        :--> PAsData PBondedStakingDatum
        :--> PAsData PBondedStakingAction
        :--> PAsData PScriptContext
        :--> PUnit
    )
pbondedPoolValidator =
  phoistAcyclic $ plam $ \params dat' act' ctx' -> unTermCont $ do
    -- Convert from Data
    _dat <- pletC $ pfromData dat'
    act <- pletC $ pfromData act'
    _ctx <- pletC $ pfromData ctx'
    -- Retrieve fields from parameters
    ctxF <- tcont $ pletFields @'["txInfo"] $ pfromData ctx'
    _paramsF <- tcont $ pletFields @'["admin"] params
    -- Match on redeemer and execute the corresponding logic
    pure $ pmatch act $ \case
      PAdminAct n' ->
        adminActLogic ctxF.txInfo params $ pfield @"_0" # n'
      PStakeAct _pair' -> stakeActLogic
      PWithdrawAct _pkh' -> withdrawActLogic
      PCloseAct _ -> closeActLogic
    
-- The pool operator calculates the new available size left 
adminActLogic ::
  forall (s :: S) .
  Term s PTxInfo ->
  Term s PBondedPoolParams ->
  Term s PNatural ->
  Term s PUnit
adminActLogic txInfo params _sizeLeft = unTermCont $ do
  -- Retrieve fields from parameters
  txInfoF <- tcont $ pletFields @'["signatories", "validRange"] txInfo
  paramsF <- tcont $ pletFields @'["admin"] params
  -- We check that the transaction was signed by the pool operator
  guardC "transaction not signed by admin" $
    signedByAdmin txInfoF.signatories paramsF.admin 
  -- We check that the transaction occurs during a bonding period
  period <- getPeriod txInfoF.validRange params
  guardC "admin deposit not done in bonding period" $
  -- TODO: Validate that pool's state and list entries are updated correctly
    isBondingPeriod period
  where isBondingPeriod :: Term s PPeriod -> Term s PBool
        isBondingPeriod period = pmatch period $ \case
          BondingPeriod -> pconstant True
          _ -> pconstant False
  
stakeActLogic :: forall (s :: S) . Term s PUnit
stakeActLogic = pconstant ()

withdrawActLogic :: forall (s :: S) . Term s PUnit
withdrawActLogic = pconstant ()

closeActLogic :: forall (s :: S) . Term s PUnit
closeActLogic = pconstant ()

hbondedPoolValidator :: BondedPoolParams -> Validator
hbondedPoolValidator bondedPoolParams =
  mkValidator $
    punsafeCoerce $
      pbondedPoolValidator
        # pconstant bondedPoolParams
        
signedByAdmin :: forall (s :: S) .
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PPubKeyHash ->
  Term s PBool
signedByAdmin ls pkh = pelem # pdata pkh # ls

{- A newtype used internally for encoding different periods.

   Depending on the pool's parameters, a certain period can either be:

   0. UnavailablePeriod: The pool has not started yet and no actions are
      permitted.
   1. DepositWithdrawPeriod: A user can both stake and deposit
   2. BondingPeriod: Only admin actions are allowed
   3. OnlyWithdrawPeriod: Users can only withdraw, this happens once in the
      lifetime of a pool, before closing.
   4. ClosingPeriod: The admin can withdraw the remaining funds and close the
      pool
-}
data PPeriod (s :: S)
  = UnavailablePeriod
  | DepositWithdrawPeriod
  | BondingPeriod
  | OnlyWithdrawPeriod
  | ClosingPeriod
  deriving stock (GHC.Generic, Eq)
  deriving anyclass (Generic, PlutusType)
  
getPeriod :: forall (s :: S) .
  Term s PPOSIXTimeRange ->
  Term s PBondedPoolParams ->
  TermCont s (Term s PPeriod)
getPeriod txTimeRange params = do
  -- Retrieve fields
  paramsF <- tcont $ pletFields
    @'["iterations", "start", "end", "userLength", "bondingLength"]
    params
  -- Convert from data
  iterations' <- pletDataC paramsF.iterations
  let iterations :: Term s PInteger
      iterations = pto iterations'
  start <- pletDataC paramsF.start
  end <- pletDataC paramsF.end
  userLength <- pletDataC paramsF.userLength
  bondingLength <- pletDataC paramsF.bondingLength
  -- We define the periodic intervals in which the Deposit/Withdrawal and
  -- Bonding will happen
  period <- pletC $ punsafeCoerce $ pto userLength + pto bondingLength
  let depositWithdrawal = PPeriodicInterval {
      piBaseOffset = start
      , piPeriod = period
      , piStartOffset = pconstant 0
      , piEndOffset = userLength
      , piMaxCycles = iterations'}
      bonding = depositWithdrawal {
      piStartOffset = userLength
      , piEndOffset = bondingLength
      }
  piDepositWithdrawal <- pletC $ pcon depositWithdrawal
  piBonding <- pletC $ pcon bonding
  pure $ pnestedIf [
   pintervalTo start `pcontains` txTimeRange
     >: pcon UnavailablePeriod
   , pperiodicContains # piDepositWithdrawal # txTimeRange
     >: pcon DepositWithdrawPeriod
   , pperiodicContains # piBonding # txTimeRange
     >: pcon BondingPeriod
   , pcontains
        (pinterval (punsafeCoerce $ iterations * pto period + pto start) end)
        txTimeRange
     >: pcon OnlyWithdrawPeriod
   , pintervalFrom end `pcontains` txTimeRange
     >: pcon ClosingPeriod
   ] $ ptraceError "the transaction's range does not belong to any valid period"