{-# LANGUAGE UndecidableInstances #-}

module UnbondedStaking.UnbondedPool (
  punbondedPoolValidator,
  punbondedPoolValidatorUntyped,
) where

import Control.Monad((<=<))
import GHC.Records (getField)
import InductiveLogic (
  doesNotConsumeUnbondedAssetGuard,
  hasListNft,
  hasStateNft,
  pointsNowhere,
  pointsTo,
 )

import Plutarch.Api.V1 (
  PAddress,
  PDatumHash,
  PMaybeData (PDJust, PDNothing),
  PPOSIXTimeRange,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose,
  PTokenName (PTokenName),
  PTuple,
  PTxInfo,
  PTxOut,
 )
import Plutarch.Api.V1.Scripts (PDatum)
import Plutarch.Builtin (pforgetData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr (HRec)
import Plutarch.Rational (PRational (PRational))
import Plutarch.Unsafe (punsafeCoerce)

import PInterval (
  pcontains,
  pintervalTo,
  pperiodicContains,
  PPeriodicInterval(..),
 )
import PNatural (
  PNatRatio (PNatRatio),
  PNatural (PNatural),
  natZero,
  ratZero,
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
  PAssetClass,
  PMintingAction(PMintHead, PMintInBetween, PMintEnd),
  passetClass,
  PPeriod,
  PTxInfoFields,
  PTxInfoHRec,
  PTxInInfoHRec,
  PTxInInfoFields,
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
  PBoolData,
  PEntry,
  PEntryFields,
  PEntryHRec,
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
  getCoWithDatum,
  getDatum,
  getDatumHash,
  getInput,
  getTokenName,
  guardC,
  oneWith,
  parseStakingDatum,
  peq,
  pfalse,
  pgt,
  pletC,
  pletDataC,
  pmatchC,
  pnestedIf,
  ptryFromUndata,
  ptrue,
  signedBy,
  signedOnlyBy,
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
adminActLogic txInfoF paramsF purpose datum adminActParamsF = unTermCont $ do
  -- We check that the transaction was signed by the pool operator
  guardC "adminActLogic: transaction not signed by admin" $
    signedBy txInfoF.signatories paramsF.admin
  -- We get the input's address
  inputF <-
    tcont . pletFields @'["outRef", "resolved"]
      =<< getInput purpose txInfoF.inputs
  inputResolvedF <-
    tcont . pletFields @["address", "value", "datumHash"] $ inputF.resolved
  let
    poolAddr :: Term s PAddress
    poolAddr = inputResolvedF.address
    txInfoDataF :: Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum)))
    txInfoDataF = getField @"data" txInfoF
  -- We make sure that the input's Datum is updated correctly for each Datum
  -- constructor
  pure . pmatch datum $ \case
    PStateDatum _ ->
      ptraceError
        "adminActLogic: update failed because a wrong \
        \datum constructor was provided"
    PEntryDatum oldEntryRecord -> unTermCont $ do
      -- Retrieve fields from oldEntry
      oldEntry <- pletC $ pfield @"_0" # oldEntryRecord
      oldEntryF <- tcont $ pletFields @PEntryFields $ oldEntry
      -- Ensure pool is open
      guardC
        "adminActLogic: update failed because pool is not open"
        $ toPBool # oldEntryF.open
      -- Get updated entry
      entryTok <- pletC $ getTokenName paramsF.assocListCs inputResolvedF.value
      newEntryF <- getOutputEntry poolAddr entryTok txInfoDataF txInfoF.outputs
      -- Validate new Entry datum
      entryChangedGuard "key" (oldEntryF.key #== newEntryF.key)
      entryChangedGuard
        "deposited"
        (oldEntryF.deposited #== newEntryF.deposited)
      guardC
        "adminActLogic: update failed because entry field 'newDeposit' \
        \is not zero"
        $ newEntryF.newDeposit #== natZero
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
stakeActLogic txInfoF paramsF purpose datum stakeActParamsF = unTermCont $ do
  -- Get the input being spent
  spentInput <-
    tcont . pletFields @PTxInInfoFields
      =<< getInput purpose txInfoF.inputs
  let poolAddr :: Term s PAddress
      poolAddr = pfield @"address" # spentInput.resolved
    -- Check that input spent is not an asset UTXO (user cannot withdraw)
  doesNotConsumeUnbondedAssetGuard datum
  -- Validate holder's signature
  guardC "stakeActLogic: tx not exclusively signed by the stake-holder" $
    signedOnlyBy txInfoF.signatories stakeActParamsF.pubKeyHash
  -- Check that amount is positive
  let stakeAmt :: Term s PNatural
      stakeAmt = pfromData stakeActParamsF.stakeAmount
  guardC "stakeActLogic: stake amount is not positive or within bounds" $
    natZero #<= stakeAmt
  -- Get asset output of the transaction (new locked stake)
  assetOutput <-
    (tcont . pletFields @'["value"])
      =<< fmap
        fst
        ( getCoWithDatum
            poolAddr
            isAssetDatum
            txInfoF.outputs
            $ getField @"data" txInfoF
        )
  -- Check that the correct amount of the assetclass is locked
  unbondedAsset <-
    tcont . pletFields @'["currencySymbol", "tokenName"] $
      paramsF.unbondedAssetClass
  guardC "stakeActLogic: amount deposited does not match redeemer's amount" $
    oneWith # (peq # unbondedAsset.currencySymbol)
      # (peq # unbondedAsset.tokenName)
      # (peq #$ pto $ pfromData stakeActParamsF.stakeAmount)
      # assetOutput.value
  pure . pmatch stakeActParamsF.maybeMintingAction $ \case
    -- If some minting action is provided, this is a new stake and inductive
    -- conditions must be checked
    PDJust mintAct -> unTermCont $ do
      -- Check that minted value is a list entry (minting policy is run)
      guardC "stakeActLogic: failure when checking minted value in minting tx" $
        hasListNft paramsF.assocListCs txInfoF.mint
      -- Check inductive conditions and business logic
      newStakeLogic
        txInfoF
        paramsF
        spentInput
        datum
        stakeActParamsF.pubKeyHash
        stakeActParamsF.stakeAmount
        $ pfield @"_0" # mintAct
    -- If no minting action is provided, this is a stake update
    PDNothing _ -> unTermCont $ do
      -- A list token should *not* be minted
      guardC
        "stakeActLogic: failure when checking minted value in non-minting \
        \ tx"
        $ pnot #$ hasListNft paramsF.assocListCs txInfoF.mint
      -- Check business logic
      updateStakeLogic
        txInfoF
        paramsF
        spentInput
        datum
        stakeActParamsF.stakeAmount
        stakeActParamsF.pubKeyHash
  where
    isAssetDatum :: Term s (PDatum :--> PBool)
    isAssetDatum = plam $ \dat' -> unTermCont $ do
      dat <-
        ptryFromUndata @PUnbondedStakingDatum
          . pforgetData
          . pdata
          $ dat'
      pure . pmatch dat $ \case
        PAssetDatum _ -> ptrue
        _ -> pfalse

-- This function validates the update of a an already existing entry in the list
updateStakeLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PUnbondedStakingDatum ->
  Term s PNatural ->
  Term s PPubKeyHash ->
  TermCont s (Term s PUnit)
updateStakeLogic txInfoF paramsF spentInputF datum stakeAmt holderPkh = do
  -- Construct some useful values for later
  stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
  stakeHolderTn <- pletC $ pcon $ PTokenName $ stakeHolderKey
  spentInputResolved <-
    tcont . pletFields @'["address", "value", "datumHash"] $
      spentInputF.resolved
  let poolAddr :: Term s PAddress
      poolAddr = spentInputResolved.address
      txInfoData = getField @"data" txInfoF
  newEntryTok <- pletC $ passetClass # paramsF.assocListCs # stakeHolderTn
  ---- FETCH DATUMS ----
  oldEntryF <- tcont . pletFields @PEntryFields =<< getEntryData datum
  newEntryF <- getOutputEntry poolAddr newEntryTok txInfoData txInfoF.outputs
  ---- BUSINESS LOGIC ----
  guardC "updateStakeLogic: update failed because pool is not open" $
    toPBool # oldEntryF.open
    #&& toPBool # newEntryF.open
  guardC "updateStakeLogic: spent entry's key does not match user's key" $
    oldEntryF.key #== stakeHolderKey
  guardC "updateStakeLogic: new entry does not have the stakeholder's key" $
    newEntryF.key #== stakeHolderKey
  guardC "updateStakeLogic: incorrect update of newDeposit" $
    newEntryF.newDeposit #== oldEntryF.newDeposit #+ stakeAmt
  guardC "updateStakeLogic: incorrect update of deposit" $
    newEntryF.deposited #== oldEntryF.deposited #+ stakeAmt
  guardC "updateStakeLogic: update increases stake beyond allowed bounds" $
    pfromData paramsF.minStake #<= newEntryF.deposited
      #&& pfromData newEntryF.deposited #<= paramsF.maxStake
  guardC
    "updateStakeLogic: update should not change rewards, totalRewards, \
    \  totalDeposited, or next fields"
    $ oldEntryF.rewards #== newEntryF.rewards
      #&& oldEntryF.totalRewards #== newEntryF.totalRewards
      #&& oldEntryF.totalDeposited #== newEntryF.totalDeposited
      #&& oldEntryF.next #== newEntryF.next

{- | This function checks all inductive conditions and makes all necessary
 business logic validation on the state/entry updates and new entries
-}
newStakeLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PUnbondedStakingDatum ->
  Term s PPubKeyHash ->
  Term s PNatural ->
  Term s PMintingAction ->
  TermCont s (Term s PUnit)
newStakeLogic txInfoF paramsF spentInputF datum holderPkh stakeAmt mintAct =
  do
    -- Construct some useful values for later
    stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
    stakeHolderTn <- pletC $ pcon $ PTokenName $ stakeHolderKey
    spentInputResolved <-
      tcont . pletFields @'["address", "value", "datumHash"] $
        spentInputF.resolved
    let poolAddr :: Term s PAddress
        poolAddr = spentInputResolved.address
        stateTn :: Term s PTokenName
        stateTn = pconstant unbondedStakingTokenName
    stateTok <- pletC $ passetClass # paramsF.nftCs # stateTn
    newEntryTok <- pletC $ passetClass # paramsF.assocListCs # stakeHolderTn
    txInfoData <- pletC $ getField @"data" txInfoF
    pure . pmatch mintAct $ \case
      PMintHead stateOutRef -> unTermCont $ do
        ---- FETCH DATUMS ----
        -- Get datum for next state
        nextStateTxOut <-
          getContinuingOutputWithNFT poolAddr stateTok txInfoF.outputs
        nextStateHash <- getDatumHash nextStateTxOut
        (nextEntryKey, _nextSizeLeft) <-
          getStateData
            =<< parseStakingDatum
            =<< getDatum nextStateHash txInfoData
        -- Get datum for current state
        (entryKey, _sizeLeft) <- getStateData datum
        -- Get datum for new list entry
        newEntry <-
          getOutputEntry poolAddr newEntryTok txInfoData txInfoF.outputs
        ---- BUSINESS LOGIC ----
        newEntryGuard paramsF newEntry stakeAmt stakeHolderKey
        ---- INDUCTIVE CONDITIONS ----
        -- Validate that spentOutRef is the state UTXO and matches redeemer
        guardC "newStakeLogic (mintHead): spent input is not the state UTXO" $
          hasStateNft
            paramsF.nftCs
            (pconstant unbondedStakingTokenName)
            spentInputResolved.value
        guardC
          "newStakeLogic (mintHead): spent input does not match redeemer \
          \input"
          $ spentInputF.outRef #== pfield @"_0" # stateOutRef
        -- Validate next state
        guardC
          "newStakeLogic (mintHead): next pool state does not point to new \
          \ entry"
          $ nextEntryKey `pointsTo` newEntry.key
        -- Validate list order and links
        pure . pmatch entryKey $ \case
          -- We are shifting the current head forwards
          PDJust currentEntryKey' -> unTermCont $ do
            currentEntryKey <- pletC $ pfromData $ pfield @"_0" # currentEntryKey'
            -- Validate order of entries
            guardC
              "newStakeLogic (mintInBetween): new entry's key should be \
              \strictly less than  current entry"
              $ pfromData newEntry.key #< currentEntryKey
            -- The new entry should point to the current entry
            guardC
              "newStakeLogic (mintInBetween): new entry should point to \
              \current entry"
              $ newEntry.next `pointsTo` currentEntryKey
          -- This is the first stake of the pool
          PDNothing _ -> unTermCont $ do
            -- The new entry should *not* point to anything
            guardC
              "newStakeLogic (mintInBetween): new entry should not point to \
              \anything"
              $ pointsNowhere newEntry.next
      PMintInBetween outRefs -> unTermCont $ do
        ---- FETCH DATUMS ----
        entriesRefs <-
          tcont $ pletFields @'["previousEntry", "currentEntry"] outRefs
        -- Get datum for prevEntry
        prevEntry <- tcont . pletFields @PEntryFields =<< getEntryData datum
        -- Get datum for prevEntryUpdated
        let prevEntryTok :: Term s PAssetClass
            prevEntryTok =
              getTokenName
                params.assocListCs
                spentInputResolved.value
        prevEntryUpdated <-
          getOutputEntry poolAddr prevEntryTok txInfoData txInfo.outputs
        -- Get datum for new list entry
        newEntry <- getOutputEntry poolAddr newEntryTok txInfoData txInfo.outputs
        -- Get the current entry's key
        currEntryKey <- pure . pmatch prevEntry.next $ \case
          PDJust key -> pfield @"_0" # key
          PDNothing _ ->
            ptraceError
              "newStakeLogic (mintEnd): the previous \
              \ entry does not point to another entry"
        ---- BUSINESS LOGIC ----
        -- Validate initialization of new entry
        newEntryGuard params newEntry stakeAmt stakeHolderKey
        -- Previous entry should keep the same values when updated
        equalEntriesGuard prevEntry prevEntryUpdated
        ---- INDUCTIVE CONDITIONS ----
        -- Validate that previousEntry is a list entry and matches redeemer
        guardC "newStakeLogic (mintEnd): spent input is not an entry" $
          hasListNft params.assocListCs spentInputResolved.value
        guardC
          "newStakeLogic (mintEnd): spent input is not the same as input in \
          \ redeemer"
          $ spentInput.outRef #== entriesRefs.previousEntry
        -- Previous entry should now point to the new entry
        guardC
          "newStakeLogic (mintEnd): the previous entry should point to the \
          \new entry"
          $ prevEntryUpdated.next `pointsTo` newEntry.key
        -- And new entry should point to the current entry
        guardC
          "newStakeLogic (mintEnd): the new entry should point to the \
          \current entry"
          $ newEntry.next `pointsTo` currEntryKey
        -- Validate entries' order
        guardC
          "newStakeLogic (mintEnd): failed to validate order in previous, \
          \current and new entry"
          $ pfromData prevEntry.key #< newEntry.key
            #&& newEntry.key #< currEntryKey
      PMintEnd listEndOutRef' -> unTermCont $ do
        ---- FETCH DATUMS ----
        listEndOutRef <- pletC $ pfield @"_0" # listEndOutRef'
        -- Get datum for endEntry
        endEntry <- tcont . pletFields @PEntryFields =<< getEntryData datum
        -- Get datum for endEntryUpdated
        let endEntryTok :: Term s PAssetClass
            endEntryTok =
              getTokenName
                params.assocListCs
                spentInputResolved.value
        endEntryUpdated <-
          getOutputEntry poolAddr endEntryTok txInfoData txInfo.outputs
        -- Get datum for new list entry
        newEntry <- getOutputEntry poolAddr newEntryTok txInfoData txInfo.outputs
        ---- BUSINESS LOGIC ----
        -- Validate initialization of new entry
        newEntryGuard params newEntry stakeAmt stakeHolderKey
        -- End entry should keep the same values when updated
        equalEntriesGuard endEntry endEntryUpdated
        ---- INDUCTIVE CONDITIONS ----
        -- Validate that endEntry is a list entry and matches redeemer
        guardC "newStakeLogic (mintEnd): spent input is not an entry" $
          hasListNft params.assocListCs spentInputResolved.value
        guardC
          "newStakeLogic (mintEnd): spent input is not the same as input in \
          \redeemer"
          $ spentInput.outRef #== listEndOutRef
        -- End entry should point nowhere
        guardC "newStakeLogic (mintEnd): end should point nowhere" $
          pointsNowhere endEntry.next
        -- Updated end entry (no longer end) should point to new entry
        guardC
          "newStakeLogic (mintEnd): updated end should point to new end \
          \entry"
          $ endEntryUpdated.next `pointsTo` newEntry.key
        -- New entry (new end) should point nowhere
        guardC "newStakeLogic (mintEnd): new end entry should not point anywhere" $
          pointsNowhere newEntry.next
        -- Validate entries' order
        guardC
          "newStakeLogic (mintEnd): new entry's key should come after end \
          \entry"
          $ pfromData endEntryUpdated.key #< pfromData newEntry.key

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
  inputF <-
    tcont . pletFields @'["outRef", "resolved"]
      =<< getInput purpose txInfoF.inputs
  inputResolvedF <-
    tcont . pletFields @["address", "value", "datumHash"] $ inputF.resolved
  let
    poolAddr :: Term s PAddress
    poolAddr = inputResolvedF.address
    txInfoDataF :: Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum)))
    txInfoDataF = getField @"data" txInfoF

  -- We make sure that the input's Datum is updated correctly for each Datum
  -- constructor
  pure . pmatch inputStakingDatum $ \case
    PStateDatum oldStateRecord -> unTermCont $ do
      oldStateF <- tcont $ pletFields @'["_0", "_1"] oldStateRecord

      -- We obtain the asset class of the NFT
      let cs = paramsF.nftCs
          tn = pconstant unbondedStakingTokenName
          ac = passetClass # cs # tn

      -- We retrieve the continuing output's datum
      coOutput <- getContinuingOutputWithNFT poolAddr ac txInfoF.outputs
      coOutputDatumHash <- getDatumHash coOutput
      coOutputDatum <- getDatum coOutputDatumHash $ getField @"data" txInfoF
      coOutputStakingDatum <- parseStakingDatum @PUnbondedStakingDatum coOutputDatum

      -- Get new state
      PStateDatum state <- pmatchC coOutputStakingDatum
      stateF <- tcont $ pletFields @'["_0", "_1"] state

      -- Check conditions
      guardC "closeActLogic: update failed because of list head change" $
        oldStateF._0 #== stateF._0
      guardC
        "closeActLogic: update failed because the pool state is not closed"
        $ pnot # (toPBool # stateF._1)
    PEntryDatum oldEntryRecord -> unTermCont $ do
      -- Retrieve fields from oldEntry
      oldEntry <- pletC $ pfield @"_0" # oldEntryRecord
      oldEntryF <- tcont $ pletFields @PEntryFields oldEntry

      -- Ensure pool is open
      guardC
        "closeActLogic: update failed because pool is not open"
        $ toPBool # oldEntryF.open

      -- Get updated entry
      entryTok <-
        pletC $ getTokenName paramsF.assocListCs inputResolvedF.value
      newEntryF <-
        getOutputEntry poolAddr entryTok txInfoDataF txInfoF.outputs

      -- Validate new Entry datum
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

-- Retrieves state fields from staking datum
getStateData ::
  forall (s :: S).
  Term s PUnbondedStakingDatum ->
  TermCont s (Term s (PMaybeData PByteString), Term s PBoolData)
getStateData datum = do
  record <- tcont . pletFields @["_0", "_1"] . pmatch datum $ \case
    PStateDatum record -> record
    _ -> ptraceError "getStateData: datum is not PStateDatum"
  pure (pfromData record._0, pfromData record._1)

-- Retrieves entry fields from staking datum
getEntryData ::
  forall (s :: S).
  Term s PUnbondedStakingDatum ->
  TermCont s (Term s PEntry)
getEntryData datum =
  pure . pmatch datum $ \case
    PEntryDatum entry -> pfield @"_0" # entry
    _ -> ptraceError "getEntryDatum: datum is not PEntryDatum"

-- getOutputState ::
--   forall (s :: S).
--   Term s PAddress ->
--   Term s PAssetClass ->
--   Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum))) ->
--   Term s (PBuiltinList (PAsData PTxOut)) ->
--   TermCont s (Term s (PMaybeData PByteString), Term s PBoolData)
--   -- TermCont s (
--   --   HRec
--   --   '[ HField s "maybeEntryName" (PMaybeData PByteString)
--   --    , HField s "open" PBoolData
--   --    ]
--   -- )
-- getOutputState poolAddr ac txInfoData =
--   tcont . pletFields @'["maybeEntryName", "open"]
--     <=< getStateData
--     <=< parseStakingDatum
--     <=< flip getDatum txInfoData
--     <=< getDatumHash
--     <=< getContinuingOutputWithNFT poolAddr ac

getOutputEntry ::
  forall (s :: S).
  Term s PAddress ->
  Term s PAssetClass ->
  Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum))) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  TermCont s (PEntryHRec s)
getOutputEntry poolAddr ac txInfoData =
  tcont . pletFields @PEntryFields
    <=< getEntryData
    <=< parseStakingDatum
    <=< flip getDatum txInfoData
    <=< getDatumHash
    <=< getContinuingOutputWithNFT poolAddr ac

-- This function validates the fields for a freshly minted entry
newEntryGuard ::
  forall (s :: S).
  PUnbondedPoolParamsHRec s ->
  PEntryHRec s ->
  Term s PNatural ->
  Term s PByteString ->
  TermCont s (Term s PUnit)
newEntryGuard paramsF newEntryF stakeAmt stakeHolderKey = do
  guardC
    "newEntryGuard: incorrect init. of newDeposit and deposit fields \
    \in first stake"
    $ newEntryF.newDeposit #== stakeAmt
      #&& newEntryF.deposited #== stakeAmt
  guardC "newEntryGuard: new entry does not have the stakeholder's key" $
    newEntryF.key #== stakeHolderKey
  guardC "newEntryGuard: new entry's stake not within stake bounds" $
    pfromData paramsF.minStake #<= newEntryF.deposited
      #&& pfromData newEntryF.deposited #<= paramsF.maxStake
  guardC
    "newEntryGuard: new entry's rewards, totalRewards, and totalDeposited \
    \fields not initialized to zero"
    $ newEntryF.rewards #== ratZero
      #&& newEntryF.totalRewards #== natZero
      #&& newEntryF.totalDeposited #== natZero

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
