{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module UnbondedStaking.UnbondedPool (
  punbondedPoolValidator,
  punbondedPoolValidatorUntyped,
) where

import Data.Natural (
  PNatural,
 )
import GHC.Records (getField)

import Plutarch.Api.V1 (
  PAddress,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose,
  PTxInfo,
 )
import Plutarch.Api.V1.Scripts (PDatum)
import Plutarch.Builtin (pforgetData)
import Plutarch.Unsafe (punsafeCoerce)

import Settings (unbondedStakingTokenName)
import Types (
  passetClass,
 )
import UnbondedStaking.Types (
  PUnbondedPoolParams,
  PUnbondedStakingAction (
    PAdminAct,
    PCloseAct,
    PStakeAct,
    PWithdrawAct
  ),
  PUnbondedStakingDatum (PAssetDatum, PEntryDatum, PStateDatum),
 )
import Utils (
  getContinuingOutputWithNFT,
  getDatum,
  getDatumHash,
  getInput,
  guardC,
  pconstantC,
  pletC,
  -- pletDataC,
  pmatchC,
  -- pnestedIf,
  ptryFromUndata,
  -- (>:),
 )

punbondedPoolValidator ::
  forall (s :: S).
  Term
    s
    ( PUnbondedPoolParams
        :--> PUnbondedStakingDatum
        :--> PUnbondedStakingAction
        :--> PScriptContext
        :--> PUnit
    )
punbondedPoolValidator =
  phoistAcyclic $
    plam $ \params dat act ctx -> unTermCont $ do
      -- Retrieve fields from parameters
      ctxF <- tcont $ pletFields @'["txInfo", "purpose"] ctx
      -- Match on redeemer and execute the corresponding logic
      pure $
        pmatch act $ \case
          PAdminAct dataRecord ->
            let totalRewards = pfield @"_0" # dataRecord
                totalDeposited = pfield @"_1" # dataRecord
             in adminActLogic
                  ctxF.txInfo
                  ctxF.purpose
                  params
                  dat
                  totalRewards
                  totalDeposited
          PStakeAct _pair' -> stakeActLogic
          PWithdrawAct _pkh' -> withdrawActLogic
          PCloseAct _ -> closeActLogic ctxF.txInfo params

-- Untyped version to be serialised. This version is responsible for verifying
-- that the parameters (pool params, datum and redeemer) have the proper types.
-- The script context should always be safe.
punbondedPoolValidatorUntyped ::
  forall (s :: S).
  Term
    s
    ( PData
        :--> PData
        :--> PData
        :--> PData
        :--> PUnit
    )
punbondedPoolValidatorUntyped = plam $ \pparams dat act ctx ->
  punbondedPoolValidator
    # unTermCont (ptryFromUndata pparams)
    # unTermCont (ptryFromUndata dat)
    # unTermCont (ptryFromUndata act)
    # punsafeCoerce ctx

-- The pool operator calculates the new available size left
-- TODO: Besides the logic related to updating the entries, there should also
-- be a check that makes sure the admin is not _reducing_ the stakes or the
-- rewards
adminActLogic ::
  forall (s :: S).
  Term s PTxInfo ->
  Term s PScriptPurpose ->
  Term s PUnbondedPoolParams ->
  Term s PUnbondedStakingDatum ->
  Term s PNatural ->
  Term s PNatural ->
  Term s PUnit
adminActLogic
  txInfo purpose params inputStakingDatum tRewards tDeposited = unTermCont $ do
    -- Retrieve fields from parameters
    txInfoF <-
      tcont $
        pletFields
          @'["inputs", "outputs", "signatories", "validRange", "data"]
          txInfo
    paramsF <- tcont $ pletFields @'["admin"] params
    -- We check that the transaction was signed by the pool operator
    guardC "transaction not signed by admin" $
      signedByAdmin txInfoF.signatories paramsF.admin
    -- We check that the transaction occurs during a bonding period
    -- We don't validate this for the demo, otherwise testing becomes
    -- too difficult
    -- let period = getPeriod # txInfoF.validRange # params
    -- guardC "admin deposit not done in bonding period" $
    --  isBondingPeriod period
    -- We get the input's address
    input <- getInput purpose txInfoF.inputs
    inputResolved <- pletC $ pfield @"resolved" # input
    let inputAddress :: Term s PAddress
        inputAddress = pfield @"address" # inputResolved
    -- We make sure that the input's Datum is updated correctly for each Datum
    -- constructor
    pure $
      pmatch inputStakingDatum $ \case
        PStateDatum oldState -> unTermCont $ do
          oldStateF <- tcont $ pletFields @'["_0", "_1"] oldState
          -- We obtain the asset class of the NFT
          let cs = pfield @"nftCs" # params
              tn = pconstant unbondedStakingTokenName
              ac = passetClass # cs # tn
          -- We retrieve the continuing output's datum
          coOutput <- getContinuingOutputWithNFT inputAddress ac txInfoF.outputs
          coOutputDatumHash <- getDatumHash coOutput
          -- coOutputDatum <- getDatum coOutputDatumHash (pfield @"data" # txInfo)
          coOutputDatum <- getDatum coOutputDatumHash $ getField @"data" txInfoF
          coOutputStakingDatum <- parseStakingDatum coOutputDatum
          -- Get new state
          PStateDatum state <- pmatchC coOutputStakingDatum
          stateF <- tcont $ pletFields @'["_0", "_1"] state
          -- Check conditions
          guardC "adminActLogic: update failed because of list head change" $
            stateF._0 #== oldStateF._0
          guardC
            "adminActLogic: update failed because new size was not updated \
            \correctly"
            $ stateF._1 #== sizeLeft
        PEntryDatum oldEntry' -> unTermCont $ do
          -- Retrieve fields from oldEntry
          oldEntry <- pletC $ pfield @"_0" # oldEntry'
          _oldEntryF <-
            tcont $
              pletFields
                @'[ "key"
                  , "sizeLeft"
                  , "newDeposit"
                  , "deposited"
                  , "staked"
                  , "rewards"
                  , "value"
                  , "next"
                  ]
                oldEntry
          -- We obtain the asset class of the NFT
          let _cs = pfield @"assocListCs" # params
              _tn = pconstant unbondedStakingTokenName
              _ac = passetClass # _cs # _tn
          -- TODO: Verify that most fields are kept intact, that size is updated
          -- and that interests are calculated correctly
          pconstantC ()
        PAssetDatum _ ->
          ptraceError
            "adminActLogic: update failed because a wrong \
            \datum constructor was provided"
    -- where
    --   __isBondingPeriod :: Term s PPeriod -> Term s PBool
    --   __isBondingPeriod period = pmatch period $ \case
    --     BondingPeriod -> pconstant True
    --     _ -> pconstant False


stakeActLogic :: forall (s :: S). Term s PUnit
stakeActLogic = pconstant ()

withdrawActLogic :: forall (s :: S). Term s PUnit
withdrawActLogic = pconstant ()

closeActLogic ::
  forall (s :: S).
  Term s PTxInfo ->
  Term s PUnbondedPoolParams ->
  Term s PUnit
closeActLogic _ _ = pconstant ()

-- Helper functions for the different logics

signedByAdmin ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PPubKeyHash ->
  Term s PBool
signedByAdmin ls pkh = pelem # pdata pkh # ls

parseStakingDatum ::
  forall (s :: S).
  Term s PDatum ->
  TermCont s (Term s PUnbondedStakingDatum)
parseStakingDatum datum =
  ptryFromUndata @PUnbondedStakingDatum . pforgetData . pdata $ datum
