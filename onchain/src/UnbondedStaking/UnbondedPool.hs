{-# LANGUAGE UndecidableInstances #-}

module UnbondedStaking.UnbondedPool (
  punbondedPoolValidator,
  punbondedPoolValidatorUntyped,
) where

import GHC.Records (getField)

import Plutarch (compile)
import Plutarch.Api.V1 (
  PAddress,
  PMaybeData (PDJust, PDNothing),
  PPOSIXTimeRange,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose,
  PTxInfo,
  validatorHash,
 )
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.DataRepr (HRec)
import Plutarch.Rational (PRational (PRational))
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
 )
import Plutus.V1.Ledger.Scripts (
  Validator (Validator),
  ValidatorHash,
 )

import PInterval (
  pcontains,
  pintervalTo,
  pperiodicContains,
  PPeriodicInterval(..),
 )
import PNatural (
  PNatRatio (PNatRatio),
  PNatural (PNatural),
  pCeil,
  toPNatRatio,
  toPInteger,
  toPRational,
  (#+),
  (#-),
  (#*),
 )
import PTypes (
  HField,
  PMintingAction(PMintHead, PMintInBetween, PMintEnd),
  passetClass,
  PPeriod,
  PTxInfoFields,
  PTxInfoHRec,
  unavailablePeriod,
  depositWithdrawPeriod,
  adminUpdatePeriod,
  bondingPeriod,
 )

import SingularityNet.Natural (
  NatRatio (NatRatio),
  Natural (Natural),
 )
import SingularityNet.Settings (unbondedStakingTokenName)

import UnbondedStaking.PTypes (
  PEntry,
  PEntryFields,
  PUnbondedPoolParams,
  PUnbondedPoolParamsFields,
  PUnbondedPoolParamsHRec,
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
  parseStakingDatum,
  pgt,
  pletC,
  pletDataC,
  pmatchC,
  pnestedIf,
  ptryFromUndata,
  signedBy,
  toPBool,
  (>:),
 )

import Data.Ratio (
  (%),
 )

{- The validator has two responsibilities

     1. Make sure that the list's inductive conditions are preserved

     2. Do business logic checks, which means validating the datums of the
     inputs and outputs (and therefore validate the correct outputs are
     produced)

   Some basic conditions are checked by the minting policy (see `ListNFT`), so
   the validator needs to make sure that a value was minted to be able to assume
   them.

   Specifically, the minting policy makes sure that the correct inputs are
   consumed/burnt (e.g: in a list insertion, only two list entries are consumed
   and no other NFT. Also, the minted entry's `TokenName` matches the
   signatory's PKH).
-}
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
punbondedPoolValidator = phoistAcyclic $
  plam $ \params dat act ctx -> unTermCont $ do
    -- Retrieve fields from parameters
    ctxF <- tcont $ pletFields @'["txInfo", "purpose"] ctx
    txInfoF <- tcont $ pletFields @PTxInfoFields ctxF.txInfo
    paramsF <- tcont $ pletFields @PUnbondedPoolParamsFields params
    -- Match on redeemer, check period and minted value, execute the
    -- corresponding logic
    pure $
      pmatch act $ \case
        PAdminAct dataRecord -> unTermCont $ do
          guardC "punbondedPoolValidator: wrong period for PAdminAct \
            \redeemer" $
            getUnbondedPeriod # txInfoF.validRange # params #==
              adminUpdatePeriod
          adminActParamsF <-
            tcont $ pletFields @["totalRewards", "totalDeposited"] dataRecord
          pure $
            adminActLogic
              txInfoF
              paramsF
              ctxF.purpose
              dat
              adminActParamsF
        PStakeAct dataRecord -> unTermCont $ do
          guardC "punbondedPoolValidator: wrong period for PStakeAct \
            \redeemer" $
            getUnbondedPeriod # txInfoF.validRange # params #==
              depositWithdrawPeriod
          stakeActParamsF <-
            tcont $ pletFields
            @["stakeAmount", "pubKeyHash", "maybeMintingAction"]
            dataRecord
          pure $
            stakeActLogic
              txInfoF
              paramsF
              ctxF.purpose
              dat
              stakeActParamsF
        PWithdrawAct dataRecord -> --unTermCont $ do
          -- update with new fields (minting)
          let pkh = pfield @"pubKeyHash" # dataRecord
            in withdrawActLogic pkh
        PCloseAct _ -> unTermCont $ do
          guardC "punbondedPoolValidator: wrong period for PCloseAct \
            \redeemer" $
            getUnbondedPeriod # txInfoF.validRange # params #==
              adminUpdatePeriod
          pure $ closeActLogic txInfoF paramsF ctxF.purpose dat

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

unbondedValidatorHash :: ValidatorHash
unbondedValidatorHash =
  validatorHash $ Validator $ compile punbondedPoolValidatorUntyped

punbondedValidatorAddress :: Term s PAddress
punbondedValidatorAddress =
  pconstant $ Address (ScriptCredential unbondedValidatorHash) Nothing

adminActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PUnbondedStakingDatum ->
  HRec
    '[ HField s "totalRewards" PNatural
     , HField s "totalDeposited" PNatural
     ] ->
  Term s PUnit
adminActLogic
  txInfoF
  paramsF
  purpose
  inputStakingDatum
  adminActParamsF = unTermCont $ do
    -- We check that the transaction was signed by the pool operator
    guardC "adminActLogic: transaction not signed by admin" $
      signedBy txInfoF.signatories paramsF.admin

    -- We get the input's address
    input <- getInput purpose txInfoF.inputs
    inputResolved <- pletC $ pfield @"resolved" # input
    let inputAddress :: Term s PAddress
        inputAddress = pfield @"address" # inputResolved

    -- We make sure that the input's Datum is updated correctly for each Datum
    -- constructor
    pure $
      pmatch inputStakingDatum $ \case
        PStateDatum _ ->
          ptraceError
            "adminActLogic: update failed because a wrong \
            \datum constructor was provided"
        PEntryDatum oldEntryRecord -> unTermCont $ do
          -- Retrieve fields from oldEntry
          oldEntry <- pletC $ pfield @"entry" # oldEntryRecord
          oldEntryF <- tcont $ pletFields @PEntryFields oldEntry

          -- Ensure pool is open
          guardC
            "adminActLogic: update failed because pool is not open"
            $ toPBool # oldEntryF.open

          -- Validate new Entry datum
          -- We obtain the asset class of the NFT
          let cs = paramsF.assocListCs
              tn = pconstant unbondedStakingTokenName
              ac = passetClass # cs # tn

          -- We retrieve the continuing output's datum
          coOutput <- getContinuingOutputWithNFT inputAddress ac txInfoF.outputs
          coOutputDatumHash <- getDatumHash coOutput
          coOutputDatum <- getDatum coOutputDatumHash $ getField @"data" txInfoF
          coOutputStakingDatum <- parseStakingDatum @PUnbondedStakingDatum coOutputDatum

          -- Retrieve fields from new Entry
          PEntryDatum newEntryRecord <- pmatchC coOutputStakingDatum
          newEntry <- pletC $ pfield @"entry" # newEntryRecord
          newEntryF <- tcont $ pletFields @PEntryFields newEntry

          -- Check updated values
          entryChangedGuard "key" (oldEntryF.key #== newEntryF.key)
          entryChangedGuard
            "deposited"
            (oldEntryF.deposited #== newEntryF.deposited)
          guardC
            "adminActLogic: update failed because entry field 'newDeposit' \
            \is not zero"
            $ newEntryF.newDeposit
              #== pconstant @PNatural (Natural 0)
          guardC
            "adminActLogic: update failed because entry field 'rewards' \
            \is not updatedRewards"
            $ newEntryF.rewards
              #== calculateRewards
                oldEntryF.rewards
                adminActParamsF.totalRewards
                oldEntryF.deposited
                oldEntryF.newDeposit
                adminActParamsF.totalDeposited
          guardC
            "adminActLogic: update failed because entry field 'totalRewards' \
            \is not newTotalRewards"
            $ newEntryF.totalRewards #== adminActParamsF.totalRewards
          guardC
            "adminActLogic: update failed because entry field \
            \'totalDeposited' is not newTotalDeposited"
            $ newEntryF.totalDeposited #== adminActParamsF.totalDeposited
          entryChangedGuard "open" (oldEntryF.open #== newEntryF.open)
          entryChangedGuard "next" (oldEntryF.next #== newEntryF.next)

          -- Verify output address is unchanged
          guardC
            "adminActLogic: update failed because output address is changed"
            $ pdata inputAddress #== pdata punbondedValidatorAddress

        PAssetDatum _ ->
          ptraceError
            "adminActLogic: update failed because a wrong \
            \datum constructor was provided"

stakeActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PUnbondedStakingDatum ->
  HRec
    '[ HField s "stakeAmount" PNatural
     , HField s "pubKeyHash" PPubKeyHash
     , HField s "maybeMintingAction" (PMaybeData PMintingAction)
     ] ->
  Term s PUnit
stakeActLogic
  txInfoF paramsF purpose inputStakingDatum stakeActParamsF = unTermCont $ do
    -- We check that the transaction was signed by the user
    guardC "stakeActLogic: transaction not signed by user" $
      signedBy txInfoF.signatories stakeActParamsF.pubKeyHash

    -- Validate stake amount
    guardC "stakeActLogic: stake amount not greater than zero" $
      pgt # pconstant @PNatural (Natural 0) # stakeActParamsF.stakeAmount

    -- We get the input's address and value
    input <- getInput purpose txInfoF.inputs
    inputResolved <- pletC $ pfield @"resolved" # input
    txOutF <- tcont $ pletFields @'["address", "value"] inputResolved

    -- We make sure that the input's Datum is updated correctly for each Datum
    -- constructor
    pure $
      pmatch inputStakingDatum $ \case
        PStateDatum _ ->
          ptraceError
            "stakeActLogic: update failed because a wrong \
            \datum constructor was provided"
        PEntryDatum _ ->
          -- new deposit
          -- inductive conds
          ptraceError
            "stakeActLogic: update failed because a wrong \
            \datum constructor was provided"
        PAssetDatum _ -> unTermCont $ do
          assetClassF <-
            tcont $
              pletFields
                @'["currencySymbol", "tokenName"]
                paramsF.unbondedAssetClass

          -- Ensure correct amount and asset is being staked to the validator
          guardC "" $ pdata txOutF.address #== pdata punbondedValidatorAddress
          -- guardC "" $ txOutF.value #== amount -- use fields in value map to cs and tn for amount

          -- Prevent withdrawing staked assets


withdrawActLogic ::
  forall (s :: S).
  Term s PPubKeyHash ->
  Term s PUnit
withdrawActLogic _ = pconstant ()

closeActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PUnbondedStakingDatum ->
  Term s PUnit
closeActLogic txInfoF paramsF purpose inputStakingDatum = unTermCont $ do
  -- We check that the transaction was signed by the pool operator
  guardC "closeActLogic: transaction not signed by admin" $
    signedBy txInfoF.signatories paramsF.admin

  -- We get the input's address
  input <- getInput purpose txInfoF.inputs
  inputResolved <- pletC $ pfield @"resolved" # input
  let inputAddress :: Term s PAddress
      inputAddress = pfield @"address" # inputResolved

  -- We make sure that the input's Datum is updated correctly for each Datum
  -- constructor
  pure $
    pmatch inputStakingDatum $ \case
      PStateDatum oldStateRecord -> unTermCont $ do
        oldStateF <- tcont $ pletFields @'["maybeEntryName", "open"] oldStateRecord

        -- We obtain the asset class of the NFT
        let cs = paramsF.nftCs
            tn = pconstant unbondedStakingTokenName
            ac = passetClass # cs # tn

        -- We retrieve the continuing output's datum
        coOutput <- getContinuingOutputWithNFT inputAddress ac txInfoF.outputs
        coOutputDatumHash <- getDatumHash coOutput
        coOutputDatum <- getDatum coOutputDatumHash $ getField @"data" txInfoF
        coOutputStakingDatum <- parseStakingDatum @PUnbondedStakingDatum coOutputDatum

        -- Get new state
        PStateDatum state <- pmatchC coOutputStakingDatum
        stateF <- tcont $ pletFields @'["maybeEntryName", "open"] state

        -- Check conditions
        guardC "closeActLogic: update failed because of list head change" $
          oldStateF.maybeEntryName #== stateF.maybeEntryName
        guardC
          "closeActLogic: update failed because the pool state is not closed"
          $ pnot # (toPBool # stateF.open)
      PEntryDatum oldEntryRecord -> unTermCont $ do
        -- Retrieve fields from oldEntry
        oldEntry <- pletC $ pfield @"entry" # oldEntryRecord
        oldEntryF <- tcont $ pletFields @PEntryFields oldEntry

        -- Ensure pool is open
        guardC
          "closeActLogic: update failed because pool is not open"
          $ toPBool # oldEntryF.open

        -- Validate new Entry datum
        -- We obtain the asset class of the NFT
        let cs = paramsF.assocListCs
            tn = pconstant unbondedStakingTokenName
            ac = passetClass # cs # tn

        -- We retrieve the continuing output's datum
        coOutput <- getContinuingOutputWithNFT inputAddress ac txInfoF.outputs
        coOutputDatumHash <- getDatumHash coOutput
        coOutputDatum <- getDatum coOutputDatumHash $ getField @"data" txInfoF
        coOutputStakingDatum <- parseStakingDatum @PUnbondedStakingDatum coOutputDatum

        -- Retrieve fields from new Entry
        PEntryDatum newEntryRecord <- pmatchC coOutputStakingDatum
        newEntry <- pletC $ pfield @"entry" # newEntryRecord
        newEntryF <- tcont $ pletFields @PEntryFields newEntry

        -- Check updated values
        entryChangedGuard "key" (oldEntryF.key #== newEntryF.key)
        guardC
          "closeActLogic: update failed because entry field 'rewards' \
          \is not updatedRewards"
          $ newEntryF.rewards
            #== calculateRewards
              oldEntryF.rewards
              oldEntryF.totalRewards
              oldEntryF.deposited
              oldEntryF.newDeposit
              oldEntryF.totalDeposited
        guardC
          "closeActLogic: update failed because entry field 'open' \
          \is not false"
          $ pnot # (toPBool # newEntryF.open)
      PAssetDatum _ ->
        pconstant ()

-- Helper functions for the different logics

-- | Get the BondedPool's period a certain POSIXTimeRange belongs to
getUnbondedPeriod ::
  forall (s :: S).
  Term
    s
    ( PPOSIXTimeRange
        :--> PUnbondedPoolParams
        :--> PPeriod
    )
getUnbondedPeriod = phoistAcyclic $
  plam $
    \txTimeRange params -> unTermCont $ do
      paramsF <- tcont $ pletFields @PUnbondedPoolParamsFields params
      pure $ getPeriod' txTimeRange paramsF
  where
    getPeriod' ::
      forall (s :: S).
      Term s PPOSIXTimeRange ->
      PUnbondedPoolParamsHRec s ->
      Term s PPeriod
    getPeriod' txTimeRange paramsF = unTermCont $ do
      -- Convert from data
      start <- pletDataC paramsF.start
      userLength <- pletDataC paramsF.userLength
      adminLength <- pletDataC paramsF.adminLength
      bondingLength <- pletDataC paramsF.bondingLength
      period <- pletC $
        punsafeCoerce $ pto userLength + pto adminLength + pto bondingLength
      let
        -- We define the periodic intervals in which the Deposit/Withdrawal
        -- and Bonding will happen
        depositWithdrawal =
          PPeriodicInterval
            { piBaseOffset = start
            , piPeriod = period
            , piStartOffset = pconstant 0
            , piEndOffset = userLength
            , piMaxCycles = pcon PNothing
            }
        adminUpdate =
          depositWithdrawal
            { piStartOffset = userLength
            , piEndOffset = punsafeCoerce $ pto userLength + pto adminLength
            }
        bonding =
          depositWithdrawal
            { piStartOffset = punsafeCoerce $ pto userLength + pto adminLength
            , piEndOffset = punsafeCoerce $
                pto userLength + pto adminLength + pto bondingLength
            }
      pure $
        pnestedIf
          [ pintervalTo start `pcontains` txTimeRange
              >: unavailablePeriod
          , pperiodicContains # pcon depositWithdrawal # txTimeRange
              >: depositWithdrawPeriod
          , pperiodicContains # pcon adminUpdate # txTimeRange
              >: adminUpdatePeriod
          , pperiodicContains # pcon bonding # txTimeRange
              >: bondingPeriod
          ]
          $ ptraceError
            "the transaction's range does not belong to any valid period"

calculateRewards ::
  forall (s :: S).
  Term s PNatRatio ->
  Term s PNatural ->
  Term s PNatural ->
  Term s PNatural ->
  Term s PNatural ->
  Term s PNatRatio
calculateRewards
  rewards
  totalRewards
  deposited
  newDeposit
  totalDeposited = unTermCont $ do
    let
      rewards' = pCeil # rewards
      fNumeratorSum = (deposited #+ rewards')
      fNumerator = fNumeratorSum #- newDeposit

    pure $ pmatch fNumerator $ \case
      PNothing ->
        ptraceError
          "adminLogic: invalid deposit amount"
      PJust fnum  -> unTermCont $ do
        let
          fNumeratorResult = fnum #* totalRewards
          fNumeratorResult' = toPInteger # fNumeratorResult
          totalDeposited' = toPInteger # totalDeposited
          f = toPNatRatio # (pcon $ PRational fNumeratorResult' totalDeposited')

        pure $ pmatch f $ \case
          PNothing -> ptraceError "AdminLogic: invalid rewards amount"
          PJust n  -> unTermCont $ do
            let
              numerator = toPInteger # (pCeil # (rewards #+ n))
              result = toPNatRatio # (pcon $ PRational numerator 1)
            pure $ pmatch result $ \case
              PNothing -> ptraceError "AdminLogic: invalid final rewards amount"
              PJust n  -> n

entryChangedGuard ::
  forall (s :: S).
  Term s PString ->
  Term s PBool ->
  TermCont s (Term s PUnit)
entryChangedGuard field =
  guardC
    ( "adminLogic: update failed because entry field '" <> field
        <> "' is changed"
    )
