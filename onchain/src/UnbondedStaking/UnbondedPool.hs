{-# LANGUAGE UndecidableInstances #-}

module UnbondedStaking.UnbondedPool (
  punbondedPoolValidator,
  punbondedPoolValidatorUntyped,
) where

import Control.Monad ((<=<))
import GHC.Records (getField)
import InductiveLogic (
  doesNotConsumeUnbondedAssetGuard,
  hasEntryToken,
  hasListNft,
  hasStateToken,
  pointsNowhere,
  pointsTo,
 )

import Plutarch.Api.V1 (
  PAddress,
  PDatumHash,
  PMaybeData (PDJust, PDNothing),
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose,
  PTokenName (PTokenName),
  PTuple,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Api.V1.Scripts (PDatum)
import Plutarch.Builtin (pforgetData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr (HRec)
import Plutarch.Unsafe (punsafeCoerce)

import PInterval (
  getUnbondedPeriod,
 )
import PNatural (
  PNatRatio,
  PNatural,
  mkNatRatioUnsafe,
  natZero,
  pCeil,
  ratZero,
  roundDown,
  toNatRatio,
  (#*),
  (#+),
  (#-),
 )
import PTypes (
  HField,
  PAssetClass,
  PBurningAction (PBurnHead, PBurnOther, PBurnSingle),
  PMintingAction (PMintEnd, PMintHead, PMintInBetween),
  PPeriod,
  PTxInInfoFields,
  PTxInInfoHRec,
  PTxInfoFields,
  PTxInfoHRec,
  passetClass,
  depositWithdrawPeriod,
  adminUpdatePeriod,
  bondingPeriod
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
  getCoWithDatum,
  getContinuingOutputWithNFT,
  getDatum,
  getDatumHash,
  getInput,
  getOutputSignedBy,
  getTokenCount,
  getTokenName,
  guardC,
  oneWith,
  parseStakingDatum,
  peq,
  pfalse,
  pfind,
  pletC,
  pnestedIf,
  ptrue,
  ptryFromUndata,
  punit,
  signedBy,
  signedOnlyBy,
  toPBool,
  (>:),
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
          -- guardC "punbondedPoolValidator: wrong period for PAdminAct \
          --   \redeemer" $
          --   getUnbondedPeriod # txInfoF.validRange # params #==
          --     adminUpdatePeriod
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
          -- guardC "punbondedPoolValidator: wrong period for PStakeAct \
          --   \redeemer" $
          --   getUnbondedPeriod # txInfoF.validRange # params #==
          --     depositWithdrawPeriod
          stakeActParamsF <-
            tcont $
              pletFields
                @["stakeAmount", "pubKeyHash", "maybeMintingAction"]
                dataRecord
          pure $
            stakeActLogic
              txInfoF
              paramsF
              ctxF.purpose
              dat
              stakeActParamsF
        PWithdrawAct dataRecord -> unTermCont $ do
          -- Period validation is done inside of the redeemer action
          let period = getUnbondedPeriod # txInfoF.validRange # params
          withdrawActParamsF <-
            tcont $ pletFields @["pubKeyHash", "burningAction"] dataRecord
          withdrawActLogic
            txInfoF
            paramsF
            ctxF.purpose
            dat
            withdrawActParamsF
            period
        PCloseAct _ -> unTermCont $ do
          -- guardC "punbondedPoolValidator: wrong period for PCloseAct \
          --   \redeemer" $
          --   getUnbondedPeriod # txInfoF.validRange # params #==
          --     adminUpdatePeriod
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
  let poolAddr :: Term s PAddress
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
      ---- FETCH DATUMS ----
      -- Retrieve fields from oldEntry
      oldEntry <- pletC $ pfield @"_0" # oldEntryRecord
      oldEntryF <- tcont $ pletFields @PEntryFields $ oldEntry
      -- Get updated entry
      entryTok <- pletC $ getTokenName paramsF.assocListCs inputResolvedF.value
      newEntryF <- getOutputEntry poolAddr entryTok txInfoDataF txInfoF.outputs
      ---- BUSINESS LOGIC ----
      guardC
        "adminActLogic: update failed because pool is not open"
        $ toPBool # oldEntryF.open
      guardC "adminActLogic: some fields in the given entries are not equal" $
        oldEntryF.key #== newEntryF.key
          #&& oldEntryF.deposited #== newEntryF.deposited
          #&& oldEntryF.open #== newEntryF.open
          #&& oldEntryF.next #== newEntryF.next
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
newStakeLogic txInfoF paramsF spentInputF datum holderPkh stakeAmt mintAct = do
  -- Construct some useful values for later
  stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
  stakeHolderTn <- pletC $ pcon $ PTokenName $ stakeHolderKey
  spentInputResolvedF <-
    tcont . pletFields @'["address", "value", "datumHash"] $
      spentInputF.resolved
  let poolAddr :: Term s PAddress
      poolAddr = spentInputResolvedF.address
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
      (nextEntryKey, nextOpen) <-
        getStateData
          =<< parseStakingDatum
          =<< getDatum nextStateHash txInfoData
      -- Get datum for current state
      (entryKey, open) <- getStateData datum
      -- Get datum for new list entry
      newEntryF <-
        getOutputEntry poolAddr newEntryTok txInfoData txInfoF.outputs
      ---- BUSINESS LOGIC ----
      guardC "newStakeLogic: update failed because pool state is not open" $
        toPBool # open
          #&& toPBool # nextOpen
      newEntryGuard paramsF newEntryF stakeAmt stakeHolderKey
      ---- INDUCTIVE CONDITIONS ----
      -- Validate that spentOutRef is the state UTXO and matches redeemer
      guardC "newStakeLogic (mintHead): spent input is not the state UTXO" $
        hasStateToken
          spentInputResolvedF.value
          (paramsF.nftCs, pconstant unbondedStakingTokenName)
      guardC
        "newStakeLogic (mintHead): spent input does not match redeemer \
        \input"
        $ spentInputF.outRef #== pfield @"_0" # stateOutRef
      -- Validate next state
      guardC
        "newStakeLogic (mintHead): next pool state does not point to new \
        \ entry"
        $ nextEntryKey `pointsTo` newEntryF.key
      -- Validate list order and links
      pure . pmatch entryKey $ \case
        -- We are shifting the current head forwards
        PDJust currentEntryKey' -> unTermCont $ do
          currentEntryKey <-
            pletC $ pfromData $ pfield @"_0" # currentEntryKey'
          -- Validate order of entries
          guardC
            "newStakeLogic (mintInBetween): new entry's key should be \
            \strictly less than current entry"
            $ pfromData newEntryF.key #< currentEntryKey
          -- The new entry should point to the current entry
          guardC
            "newStakeLogic (mintInBetween): new entry should point to \
            \current entry"
            $ newEntryF.next `pointsTo` currentEntryKey
        -- This is the first stake of the pool
        PDNothing _ -> unTermCont $ do
          -- The new entry should *not* point to anything
          guardC
            "newStakeLogic (mintInBetween): new entry should not point to \
            \anything"
            $ pointsNowhere newEntryF.next
    PMintInBetween outRefs -> unTermCont $ do
      ---- FETCH DATUMS ----
      entriesRefsF <-
        tcont $ pletFields @'["previousEntry", "currentEntry"] outRefs
      -- Get datum for prevEntry
      prevEntryF <- tcont . pletFields @PEntryFields =<< getEntryData datum
      -- Get datum for prevEntryUpdated
      let prevEntryTok :: Term s PAssetClass
          prevEntryTok =
            getTokenName
              paramsF.assocListCs
              spentInputResolvedF.value
      prevEntryUpdatedF <-
        getOutputEntry poolAddr prevEntryTok txInfoData txInfoF.outputs
      -- Get datum for new list entry
      newEntryF <-
        getOutputEntry poolAddr newEntryTok txInfoData txInfoF.outputs
      -- Get the current entry's key
      currEntryKey <- pure . pmatch prevEntryF.next $ \case
        PDJust key -> pfield @"_0" # key
        PDNothing _ ->
          ptraceError
            "newStakeLogic (mintEnd): the previous \
            \ entry does not point to another entry"
      ---- BUSINESS LOGIC ----
      guardC "newStakeLogic: update failed because pool state is not open" $
        toPBool # prevEntryF.open
          #&& toPBool # newEntryF.open
      -- Validate initialization of new entry
      newEntryGuard paramsF newEntryF stakeAmt stakeHolderKey
      -- Previous entry should keep the same values when updated
      equalEntriesGuard prevEntryF prevEntryUpdatedF
      ---- INDUCTIVE CONDITIONS ----
      -- Validate that previousEntry is a list entry and matches redeemer
      guardC "newStakeLogic (mintEnd): spent input is not an entry" $
        hasListNft paramsF.assocListCs spentInputResolvedF.value
      guardC
        "newStakeLogic (mintEnd): spent input is not the same as input in \
        \ redeemer"
        $ spentInputF.outRef #== entriesRefsF.previousEntry
      -- Previous entry should now point to the new entry
      guardC
        "newStakeLogic (mintEnd): the previous entry should point to the \
        \new entry"
        $ prevEntryUpdatedF.next `pointsTo` newEntryF.key
      -- And new entry should point to the current entry
      guardC
        "newStakeLogic (mintEnd): the new entry should point to the \
        \current entry"
        $ newEntryF.next `pointsTo` currEntryKey
      -- Validate entries' order
      guardC
        "newStakeLogic (mintEnd): failed to validate order in previous, \
        \current and new entry"
        $ pfromData prevEntryF.key #< newEntryF.key
          #&& newEntryF.key #< currEntryKey
    PMintEnd listEndOutRef' -> unTermCont $ do
      ---- FETCH DATUMS ----
      listEndOutRef <- pletC $ pfield @"_0" # listEndOutRef'
      -- Get datum for endEntry
      endEntryF <- tcont . pletFields @PEntryFields =<< getEntryData datum
      -- Get datum for endEntryUpdated
      let endEntryTok :: Term s PAssetClass
          endEntryTok =
            getTokenName
              paramsF.assocListCs
              spentInputResolvedF.value
      endEntryUpdated <-
        getOutputEntry poolAddr endEntryTok txInfoData txInfoF.outputs
      -- Get datum for new list entry
      newEntryF <-
        getOutputEntry poolAddr newEntryTok txInfoData txInfoF.outputs
      ---- BUSINESS LOGIC ----
      guardC "newStakeLogic: update failed because pool state is not open" $
        toPBool # endEntryF.open
          #&& toPBool # newEntryF.open
      -- Validate initialization of new entry
      newEntryGuard paramsF newEntryF stakeAmt stakeHolderKey
      -- End entry should keep the same values when updated
      equalEntriesGuard endEntryF endEntryUpdated
      ---- INDUCTIVE CONDITIONS ----
      -- Validate that endEntry is a list entry and matches redeemer
      guardC "newStakeLogic (mintEnd): spent input is not an entry" $
        hasListNft paramsF.assocListCs spentInputResolvedF.value
      guardC
        "newStakeLogic (mintEnd): spent input is not the same as input in \
        \redeemer"
        $ spentInputF.outRef #== listEndOutRef
      -- End entry should point nowhere
      guardC "newStakeLogic (mintEnd): end should point nowhere" $
        pointsNowhere endEntryF.next
      -- Updated end entry (no longer end) should point to new entry
      guardC
        "newStakeLogic (mintEnd): updated end should point to new end \
        \entry"
        $ endEntryUpdated.next `pointsTo` newEntryF.key
      -- New entry (new end) should point nowhere
      guardC "newStakeLogic (mintEnd): new end entry should not point anywhere" $
        pointsNowhere newEntryF.next
      -- Validate entries' order
      guardC
        "newStakeLogic (mintEnd): new entry's key should come after end \
        \entry"
        $ pfromData endEntryUpdated.key #< pfromData newEntryF.key

withdrawActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PUnbondedStakingDatum ->
  HRec
    '[ HField s "pubKeyHash" PPubKeyHash
     , HField s "burningAction" PBurningAction
     ] ->
  Term s PPeriod ->
  TermCont s (Term s PUnit)
withdrawActLogic txInfoF paramsF purpose datum withdrawActParamsF period = do
  -- Construct some useful values for later
  holderKey <- pure $ pblake2b_256 # pto (pfromData withdrawActParamsF.pubKeyHash)
  entryTn <- pletC . pcon . PTokenName $ holderKey
  -- Get the input being spent
  spentInputF <-
    tcont . pletFields @PTxInInfoFields
      =<< getInput purpose txInfoF.inputs
  spentInputResolvedF <- tcont . pletFields @'["value"] $ spentInputF.resolved
  -- Validate holder's signature
  guardC "withdrawActLogic: tx not exclusively signed by the stake-holder" $
    signedOnlyBy txInfoF.signatories withdrawActParamsF.pubKeyHash
  -- Get amount staked from output
  withdrawnAmt <-
    pure . getTokenCount paramsF.unbondedAssetClass
      <=< (\txOut -> pure $ pfield @"value" # txOut)
      <=< getOutputSignedBy withdrawActParamsF.pubKeyHash
      $ txInfoF.outputs
  -- Validate the asset input is effectively an asset UTXO
  let assetCheck :: Term s PUnit
      assetCheck = unTermCont $ do
        datum <-
          parseStakingDatum @PUnbondedStakingDatum
            <=< flip getDatum (getField @"data" txInfoF)
            <=< getDatumHash
            $ spentInputF.resolved
        pure $
          pmatch datum $ \case
            PAssetDatum _ -> punit
            _ -> ptraceError "withdrawActLogic: expected asset input"
      -- We validate the entry when consuming it
      entryCheck :: Term s PTxOutRef -> Term s PUnit
      entryCheck entryOutRef = unTermCont $ do
        guardC
          "withdrawActLogic: spent entry is not an entry (no List \
          \NFT)"
          $ hasListNft paramsF.assocListCs spentInputResolvedF.value
        guardC
          "withdrawActLogic: spent entry does not match redeemer \
          \TxOutRef"
          $ pdata entryOutRef #== spentInputF.outRef
        guardC "withdrawActLogic: entry does not belong to user" $
          hasEntryToken
            spentInputResolvedF.value
            (paramsF.assocListCs, entryTn)
  -- Check business and inductive conditions depending on redeemer
  pure . pmatch (pfromData withdrawActParamsF.burningAction) $ \case
    PBurnHead outRefs' -> unTermCont $ do
      outRefsF <- tcont . pletFields @'["state", "headEntry"] $ outRefs'
      -- We check most conditions when consuming the state UTXO
      let withdrawHeadCheck =
            withdrawHeadActLogic
              spentInputF
              withdrawnAmt
              datum
              txInfoF
              paramsF
              outRefsF.state
              outRefsF.headEntry
      pure $
        pnestedIf
          [ spentInputF.outRef #== outRefsF.state >: withdrawHeadCheck
          , spentInputF.outRef #== outRefsF.headEntry >: entryCheck outRefsF.headEntry
          ]
          assetCheck
    PBurnOther entries' -> unTermCont $ do
      entriesF <- tcont . pletFields @'["previousEntry", "burnEntry"] $ entries'
      let withdrawOtherCheck :: Term s PUnit
          withdrawOtherCheck =
            withdrawOtherActLogic
              spentInputF
              withdrawnAmt
              datum
              txInfoF
              paramsF
              entriesF.burnEntry
      pure $
        pnestedIf
          [ spentInputF.outRef #== entriesF.previousEntry >: withdrawOtherCheck
          , spentInputF.outRef #== entriesF.burnEntry >: entryCheck entriesF.burnEntry
          ]
          $ assetCheck
    PBurnSingle entry' -> unTermCont $ do
      ---- FETCH DATUMS ----
      -- Get datum for entry
      let entryOutRef = pfield @"burnEntry" # entry'
      txInfoDataF <- pletC $ getField @"data" txInfoF
      entryF <- getInputEntry entryOutRef txInfoDataF txInfoF.inputs
      -- Burning action only valid if pool is closed
      guardC "withdrawActLogic: Pool is not closed" $
        pnot # (toPBool # entryF.open)
      -- Validate period
      guardC "withdrawActLogic: wrong period for PWithdrawAct redeemer" $
        period #== depositWithdrawPeriod
        #|| period #== adminUpdatePeriod
        #|| period #== bondingPeriod
      ---- BUSINESS LOGIC ----
      -- Validate that entry key matches the key in state UTxO
      guardC "withdrawActLogic: consumed entry key does not match user's pkh" $
        entryF.key #== holderKey
      -- Validate withdrawn amount
      guardC
        "withdrawActLogic: withdrawn amount does not match stake and \
        \rewards"
        $ withdrawnAmt
          #== roundDown (toNatRatio entryF.deposited #+ entryF.rewards)
      pure $
        pnestedIf
          [ spentInputF.outRef #== pdata entryOutRef >: entryCheck entryOutRef ]
          $ assetCheck

withdrawHeadActLogic ::
  forall (s :: S).
  HRec
    '[ HField s "outRef" PTxOutRef
     , HField s "resolved" PTxOut
     ] ->
  Term s PNatural ->
  Term s PUnbondedStakingDatum ->
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PTxOutRef ->
  Term s PTxOutRef ->
  Term s PUnit
withdrawHeadActLogic spentInput withdrawnAmt datum txInfo params stateOutRef headEntryOutRef =
  unTermCont $ do
    -- Construct some useful values for later
    spentInputResolved <-
      tcont . pletFields @'["address", "value", "datumHash"] $ spentInput.resolved
    let poolAddr :: Term s PAddress
        poolAddr = spentInputResolved.address
        stateTn :: Term s PTokenName
        stateTn = pconstant unbondedStakingTokenName
    stateTok <- pletC $ passetClass # params.nftCs # stateTn
    txInfoData <- pletC $ getField @"data" txInfo
    ---- FETCH DATUMS ----
    -- Get datum for next state
    nextStateTxOut <-
      getContinuingOutputWithNFT poolAddr stateTok txInfo.outputs
    nextStateHash <- getDatumHash nextStateTxOut
    (nextEntryKey, _nextSizeLeft) <-
      getStateData
        =<< parseStakingDatum
        =<< getDatum nextStateHash txInfoData
    -- Get datum for current state
    entryKey <- getKey =<< fst <$> getStateData datum
    -- Get datum for head entry
    headEntry <- getInputEntry headEntryOutRef txInfoData txInfo.inputs
    ---- BUSINESS LOGIC ----
    -- Validate that entry key matches the key in state UTxO
    guardC "withdrawHeadActLogic: consumed entry key does not match user's pkh" $
      headEntry.key #== entryKey
    -- Validate withdrawn amount
    guardC
      "withdrawHeadActLogic: withdrawn amount does not match stake and \
      \rewards"
      $ withdrawnAmt
        #== roundDown (toNatRatio headEntry.deposited #+ headEntry.rewards)
    ---- INDUCTIVE CONDITIONS ----
    -- Validate that spentOutRef is the state UTXO and matches redeemer
    guardC "withdrawHeadActLogic: spent input is not the state UTXO" $
      spentInputResolved.value
        `hasStateToken` (params.nftCs, pconstant unbondedStakingTokenName)
    guardC "withdrawHeadActLogic: spent input does not match redeemer input" $
      spentInput.outRef #== pdata stateOutRef
    -- Validate that consumed entry is head of the list
    guardC "withdrawHeadActLogic: spent entry is not head of the list" $
      entryKey #== headEntry.key
    -- Validate next state
    guardC
      "withdrawHeadActLogic: next pool state does not point to same \
      \location as burned entry"
      $ pdata nextEntryKey #== headEntry.next

withdrawOtherActLogic ::
  forall (s :: S).
  HRec
    '[ HField s "outRef" PTxOutRef
     , HField s "resolved" PTxOut
     ] ->
  Term s PNatural ->
  Term s PUnbondedStakingDatum ->
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PTxOutRef ->
  Term s PUnit
withdrawOtherActLogic spentInput withdrawnAmt datum txInfo params burnEntryOutRef =
  unTermCont $ do
    -- Construct some useful values for later
    spentInputResolved <-
      tcont . pletFields @'["address", "value", "datumHash"] $ spentInput.resolved
    let poolAddr :: Term s PAddress
        poolAddr = spentInputResolved.address
    txInfoData <- pletC $ getField @"data" txInfo
    ---- FETCH DATUMS ----
    -- Get datum for previous entry
    prevEntry <- tcont . pletFields @PEntryFields =<< getEntryData datum
    -- Get updated datum for previous entry
    let prevEntryTok :: Term s PAssetClass
        prevEntryTok = getTokenName params.assocListCs spentInputResolved.value
    prevEntryUpdated <-
      getOutputEntry
        poolAddr
        prevEntryTok
        txInfoData
        txInfo.outputs
    -- Get datum for burned entry
    burnEntry <- getInputEntry burnEntryOutRef txInfoData txInfo.inputs
    ---- BUSINESS LOGIC ----
    -- Validate withdrawn amount
    guardC
      "withdrawOtherActLogic: withdrawn amount does not match stake and \
      \rewards"
      $ withdrawnAmt
        #== roundDown (toNatRatio burnEntry.deposited #+ burnEntry.rewards)
    ---- INDUCTIVE CONDITIONS ----
    -- Validate that spentOutRef is the previous entry and matches redeemer
    guardC "withdrawOtherActLogic: spent input is not an entry" $
      hasListNft params.assocListCs spentInputResolved.value
    -- Validate that burn entry key matches the key in previous entry
    guardC
      "withdrawOtherActLogic: consumed entry key does not match previous \
      \entry's key"
      $ prevEntry.next `pointsTo` burnEntry.key
    -- Validate updated entry
    guardC
      "withdrawOtherActLogic: updated previous entry does not point to same \
      \location as burned entry"
      $ prevEntryUpdated.next #== pdata burnEntry.next
    -- Validate other fields of updated entry (they should stay the same)
    equalEntriesGuard prevEntryUpdated prevEntry

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
  let poolAddr :: Term s PAddress
      poolAddr = inputResolvedF.address
      txInfoDataF :: Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum)))
      txInfoDataF = getField @"data" txInfoF

  -- We make sure that the input's Datum is updated correctly for each Datum
  -- constructor
  pure . pmatch inputStakingDatum $ \case
    PStateDatum oldStateRecord -> unTermCont $ do
      ---- FETCH DATUMS ----
      let stateCs = paramsF.nftCs
          stateTn = pconstant unbondedStakingTokenName
          stateTok = passetClass # stateCs # stateTn
      -- Get current state
      (stateHeadKey, _stateSize) <-
        (\s -> pure (pfromData s._0, pfromData s._1))
          <=< tcont . pletFields @'["_0", "_1"]
          $ oldStateRecord
      -- We retrieve the continuing output's datum
      (newStateHeadKey, newOpenState) <-
        getStateData
          <=< parseStakingDatum
          <=< flip getDatum txInfoDataF
          <=< getDatumHash
          <=< getContinuingOutputWithNFT poolAddr stateTok
          $ txInfoF.outputs
      ---- BUSINESS LOGIC ----
      guardC "closeActLogic: update failed because of list head change" $
        pdata stateHeadKey #== pdata newStateHeadKey
      guardC
        "closeActLogic: update failed because the pool state is not closed"
        $ pnot # (toPBool # newOpenState)
    PEntryDatum oldEntryRecord -> unTermCont $ do
      ---- FETCH DATUMS ----
      -- Retrieve fields from oldEntry
      oldEntry <- pletC $ pfield @"_0" # oldEntryRecord
      oldEntryF <- tcont $ pletFields @PEntryFields oldEntry
      -- Get updated entry
      entryTok <-
        pletC $ getTokenName paramsF.assocListCs inputResolvedF.value
      newEntryF <-
        getOutputEntry poolAddr entryTok txInfoDataF txInfoF.outputs
      ---- BUSINESS LOGIC ----
      guardC
        "closeActLogic: update failed because pool is not open"
        $ toPBool # oldEntryF.open
      guardC
        "closeActLogic: update failed because entry field 'key' is changed"
        $ oldEntryF.key #== newEntryF.key
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

getInputEntry ::
  forall (s :: S).
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum))) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (PEntryHRec s)
getInputEntry inputOutRef txInfoData inputs = do
  entry <- flip pfind inputs $
    plam $ \input ->
      let outRef :: Term s PTxOutRef
          outRef = pfield @"outRef" # input
       in pdata outRef #== pdata inputOutRef
  tcont . pletFields @PEntryFields
    <=< getEntryData
    <=< parseStakingDatum
    <=< flip getDatum txInfoData
    <=< getDatumHash
    $ pfield @"resolved" # entry

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

-- This function validates that two entries' fields are the same (with the
-- exception of fields related to the associative list)
equalEntriesGuard ::
  forall (s :: S).
  PEntryHRec s ->
  PEntryHRec s ->
  TermCont s (Term s PUnit)
equalEntriesGuard e1 e2 =
  guardC "equalEntriesGuard: some fields in the given entries are not equal" $
    e1.key #== e2.key
      #&& e1.deposited #== e2.deposited
      #&& e1.newDeposit #== e2.newDeposit
      #&& e1.rewards #== e2.rewards
      #&& e1.totalRewards #== e2.totalRewards
      #&& e1.totalDeposited #== e2.totalDeposited
      #&& e1.open #== e2.open

getKey ::
  forall (s :: S).
  Term s (PMaybeData PByteString) ->
  TermCont s (Term s PByteString)
getKey =
  pure
    . flip
      pmatch
      ( \case
          PDJust key -> pfield @"_0" # key
          PDNothing _ -> ptraceError "getKey: no key found"
      )

calculateRewards ::
  forall (s :: S).
  Term s PNatRatio ->
  Term s PNatural ->
  Term s PNatural ->
  Term s PNatural ->
  Term s PNatural ->
  Term s PNatRatio
calculateRewards rewards totalRewards deposited newDeposit totalDeposited =
  unTermCont $ do
    guardC
      "calculateRewards: totalDeposited is zero"
      $ natZero #< totalDeposited
    let lhs = mkNatRatioUnsafe (pto totalRewards) (pto totalDeposited)
        rhs = rewards #+ (toNatRatio deposited)
        rhs' = rhs #- (toNatRatio newDeposit)
    pure $
      pmatch rhs' $ \case
        PNothing ->
          ptraceError
            "calculateRewards: invalid deposit amount"
        PJust rhs'' -> unTermCont $ do
          let f = rhs'' #* lhs
              result = pCeil # (rewards #+ f)
          pure $ toNatRatio result
