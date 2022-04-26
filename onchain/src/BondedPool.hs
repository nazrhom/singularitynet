{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module BondedPool (
  pbondedPoolValidator,
  hbondedPoolValidator,
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Plutarch.Api.V1 (
  PScriptContext
  , PTxInfo
  , PPubKeyHash
  , PPOSIXTime
  , mkValidator)
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Api.V1.Time (PPOSIXTimeRange)
import Plutus.V1.Ledger.Api (
  Validator)
import Plutus.V1.Ledger.Interval(contains)
import Plutus.V1.Ledger.Interval qualified as Interval

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
  pconstantC
  , guardC
  , pletC
  )
import Data.Natural (PNatural)
import Plutarch.Api.V1.Interval

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
    dat <- pletC $ pfromData dat'
    act <- pletC $ pfromData act'
    ctx <- pletC $ pfromData ctx'
    -- Retrieve fields from parameters
    ctxF <- tcont $ pletFields @'["txInfo"] $ pfromData ctx'
    paramsF <- tcont $ pletFields @'["admin"] params
    -- Match on redeemer and execute the corresponding logic
    pure $ pmatch act $ \case
      PAdminAct n' ->
        adminActLogic ctxF.txInfo params $ pfield @"_0" # n'
      PStakeAct pair' -> stakeActLogic
      PWithdrawAct pkh' -> withdrawActLogic
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
  --guardC "admin deposit not done in bonding period" $
  --  isBondingPeriod txInfoF.validRange params
  pconstantC ()
  
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
  iterations <- pletDataC paramsF.iterations
  start <- pletDataC paramsF.start
  end <- pletDataC paramsF.end
  userLength <- pletDataC paramsF.userLength
  bondingLength <- pletDataC paramsF.bondingLength
  -- Unavailable
  --pure $ pif (Interval.to end `contains` txTimeRange) (pcon UnavailablePeriod) undefined
  pure . pcon $ ClosingPeriod
  where pletDataC x = pletC $ pfromData x