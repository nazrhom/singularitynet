{-# LANGUAGE UndecidableInstances #-}

module BondedPool (
  pbondedPoolValidator,
  pbondedPoolValidatorUntyped,
) where

import Control.Monad ((<=<))

import Plutarch.Api.V1 (
  PAddress,
  PCurrencySymbol,
  PDatumHash,
  PMaybeData (PDJust, PDNothing),
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose,
  PTokenName (PTokenName),
  PTuple,
  PTxInfo,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
  PValue,
  ptuple,
 )
import Plutarch.Unsafe (punsafeCoerce)

import PNatural (
  PNatRatio,
  PNatural,
  PNonNegative ((#*), (#+)),
  toNatRatio,
 )
import PTypes (
  PAssetClass,
  PBondedPoolParams,
  PBondedStakingAction (
    PAdminAct,
    PCloseAct,
    PStakeAct,
    PWithdrawAct
  ),
  PBondedStakingDatum (PAssetDatum, PEntryDatum, PStateDatum),
  PEntry,
  PMintingAction (PMintEnd, PMintHead, PMintInBetween),
  PBurningAction (PBurnOther, PBurnOther, PBurnHead),
  PPeriod (BondingPeriod, ClosingPeriod),
  -- bondingPeriod,
  -- depositWithdrawPeriod,
  -- onlyWithdrawPeriod,
  passetClass,
 )

-- import PInterval(
--  getBondedPeriod
--  )

import Utils (
  HField,
  PBondedPoolParamsFields,
  PBondedPoolParamsHRec,
  PEntryFields,
  PEntryHRec,
  PTxInInfoFields,
  PTxInInfoHRec,
  PTxInfoFields,
  PTxInfoHRec,
  getContinuingOutputWithNFT,
  getOutputSignedBy,
  getDatum,
  getDatumHash,
  getInput,
  getTokenName,
  guardC,
  oneWith,
  parseStakingDatum,
  pconst,
  peq,
  pfalse,
  pletC,
  pmapMaybe,
  ppairData,
  ptrue,
  ptryFromUndata,
  punit,
  pfind,
  signedBy,
  signedOnlyBy,
 )

import GHC.Records (getField)
import InductiveLogic (doesNotConsumeAssetGuard, hasListNft, hasStateToken)
import Plutarch.Api.V1.Scripts (PDatum)
import Plutarch.Api.V1.Tuple (pbuiltinPairFromTuple)
import Plutarch.Builtin (pforgetData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr (HRec)
import SingularityNet.Natural (NatRatio (NatRatio), Natural (Natural))
import SingularityNet.Settings (bondedStakingTokenName)

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
pbondedPoolValidator ::
  forall (s :: S).
  Term
    s
    ( PBondedPoolParams
        :--> PBondedStakingDatum
        :--> PBondedStakingAction
        :--> PScriptContext
        :--> PUnit
    )
pbondedPoolValidator = phoistAcyclic $
  plam $ \params dat act ctx -> unTermCont $ do
    -- Retrieve fields from parameters
    ctxF <- tcont $ pletFields @'["txInfo", "purpose"] ctx
    txInfoF <- tcont $ pletFields @PTxInfoFields ctxF.txInfo
    paramsF <- tcont $ pletFields @PBondedPoolParamsFields params
    -- Match on redeemer, check period and minted value, execute the
    -- corresponding logic
    pure $
      pmatch act $ \case
        PAdminAct n -> unTermCont $ do
          -- guardC "pbondedPoolValidator: wrong period for PAdminAct redeemer" $
          --  getBondedPeriod # txInfoF.validRange # params #== bondingPeriod
          pure $
            adminActLogic txInfoF paramsF ctxF.purpose dat $
              pfield @"_0" # n
        PStakeAct act -> unTermCont $ do
          -- guardC "pbondedPoolValidator: wrong period for PStakeAct \
          -- \redeemer" $
          --  getBondedPeriod # txInfoF.validRange # params
          --     #== depositWithdrawPeriod
          pure
            . pletFields
              @'["stakeAmount", "pubKeyHash", "maybeMintingAction"]
              act
            $ \actF ->
              stakeActLogic
                txInfoF
                paramsF
                ctxF.purpose
                dat
                actF
        PWithdrawAct act' -> unTermCont $ do
          -- period <- getBondedPeriod # txInfoF.validRange # params
          -- guardC "pbondedPoolValidator: wrong period for PWithdrawAct \
          --  \redeemer" $
          --  period #== depositWithdrawPeriod #|| onlyWithdrawPeriod
          guardC
            "pbondedPoolValidator: a token should be burned when using \
            \ PWithdrawAct"
            $ isBurningEntry txInfoF.mint paramsF.assocListCs
          act <- tcont . pletFields @'["pubKeyHash", "burningAction"] $ act'
          withdrawActLogic
            txInfoF
            paramsF
            ctxF.purpose
            dat
            act
        PCloseAct _ -> unTermCont $ do
          -- guardC "pbondedPoolValidator: wrong period for PcloseAct redeemer" $
          --   getBondedPeriod # txInfoF.validRange # params #== closingPeriod
          pure $ closeActLogic ctxF.txInfo params
  where
    isBurningEntry ::
      forall (s :: S).
      Term s PValue ->
      Term s PCurrencySymbol ->
      Term s PBool
    isBurningEntry val cs =
      oneWith # (peq # cs) # (pconst ptrue) # (peq # (-1)) # val

-- Untyped version to be serialised. This version is responsible for verifying
-- that the parameters (pool params, datum and redeemer) have the proper types.
-- The script context should always be safe.
pbondedPoolValidatorUntyped ::
  forall (s :: S).
  Term
    s
    ( PData
        :--> PData
        :--> PData
        :--> PData
        :--> PUnit
    )
pbondedPoolValidatorUntyped = plam $ \pparams dat act ctx ->
  pbondedPoolValidator
    # unTermCont (ptryFromUndata pparams)
    # unTermCont (ptryFromUndata dat)
    # unTermCont (ptryFromUndata act)
    # punsafeCoerce ctx

-- The pool operator updates the rewards for a given entry in the association
-- list
adminActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PBondedStakingDatum ->
  Term s PNatural ->
  Term s PUnit
adminActLogic txInfo params purpose inputStakingDatum sizeLeft = unTermCont $ do
  -- We check that the transaction was signed by the pool operator
  guardC "transaction not signed by admin" $
    signedBy txInfo.signatories params.admin
  -- We get the input's address
  input <-
    tcont . pletFields @'["outRef", "resolved"]
      =<< getInput purpose txInfo.inputs
  inputResolved <-
    tcont . pletFields @["address", "value", "datumHash"] $ input.resolved
  let poolAddr :: Term s PAddress
      poolAddr = inputResolved.address
  -- We get the txinfo data field for convenience
  let txInfoData :: Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum)))
      txInfoData = getField @"data" txInfo
  -- We make sure that the input's Datum is updated correctly for each Datum
  -- constructor
  pure . pmatch inputStakingDatum $ \case
    PStateDatum state' -> unTermCont $ do
      ---- FETCH DATUMS ----
      let stateCs = params.nftCs
          stateTn = pconstant bondedStakingTokenName
          stateTok = passetClass # stateCs # stateTn
      -- Get current state
      (stateHeadKey, _stateSize) <-
        (\s -> pure (pfromData s._0, pfromData s._1))
          <=< tcont . pletFields @'["_0", "_1"]
          $ state'
      -- We retrieve the continuing output's datum
      (newStateHeadKey, newStateSize) <-
        getStateData
          <=< parseStakingDatum
          <=< flip getDatum txInfoData
          <=< getDatumHash
          <=< getContinuingOutputWithNFT poolAddr stateTok
          $ txInfo.outputs
      ---- BUSINESS LOGIC ----
      guardC "adminActLogic: admin update should not update list head" $
        pdata stateHeadKey #== pdata newStateHeadKey
      guardC
        "adminActLogic: update failed because new size was not updated \
        \correctly"
        $ newStateSize #== sizeLeft
    PEntryDatum entry' -> unTermCont $ do
      ---- FETCH DATUMS ----
      -- Retrieve fields from entry
      entry <- tcont . pletFields @PEntryFields $ pfield @"_0" # entry'
      -- We get the entry' asset class
      entryTok <- pletC $ getTokenName params.assocListCs inputResolved.value
      -- Get updated entry
      newEntry <- getOutputEntry poolAddr entryTok txInfoData txInfo.outputs
      ---- BUSINESS LOGIC ----
      pure $
        pif
          (nat entry.newDeposit #== natZero)
          (noNewDepositLogic entry newEntry)
          (newDepositLogic entry newEntry)
    PAssetDatum _ ->
      ptraceError
        "adminActLogic: update failed because a wrong \
        \datum constructor was provided"
  where
    __isBondingPeriod :: Term s PPeriod -> Term s PBool
    __isBondingPeriod period = pmatch period $ \case
      BondingPeriod -> pconstant True
      _ -> pconstant False
    newDepositLogic ::
      PEntryHRec s -> PEntryHRec s -> Term s PUnit
    newDepositLogic entry newEntry = unTermCont $ do
      guardC "adminActLogic: changed invalid fields in cycle with new deposit" $
        newEntry.key #== entry.key
          #&& newEntry.deposited #== entry.deposited
          #&& newEntry.next #== entry.next
      guardC
        "adminActLogic: newDeposit was not set to zero in cycle with new\
        \ deposit"
        $ nat newEntry.newDeposit #== natZero
      guardC
        "adminActLogic: pool operator cannot reduce rewards or staked \
        \amount"
        $ nat entry.staked #<= nat newEntry.staked
          #&& pfromData entry.rewards #<= pfromData newEntry.rewards
      guardC
        "adminActLogic: failure when updating rewards in cycle with new \
        \deposit"
        $ newEntry.rewards
          #== updatedRewards entry.rewards params.interest newEntry.staked
    noNewDepositLogic ::
      PEntryHRec s -> PEntryHRec s -> Term s PUnit
    noNewDepositLogic entry newEntry = unTermCont $ do
      guardC
        "adminActLogic: changed invalid fields in cycle with no new \
        \deposit"
        $ newEntry.key #== entry.key
          #&& newEntry.deposited #== entry.deposited
          #&& newEntry.staked #== entry.staked
          #&& newEntry.next #== entry.next
      guardC
        "adminActLogic: failure when updating rewards in cycle with no \
        \new deposit"
        $ newEntry.rewards
          #== updatedRewards entry.rewards params.interest newEntry.staked
    -- Convenient conversion function
    nat :: Term s (PAsData PNatural) -> Term s PNatural
    nat = pfromData
    -- Calculate interests and update rewards
    updatedRewards ::
      Term s PNatRatio -> Term s PNatRatio -> Term s PNatural -> Term s PNatRatio
    updatedRewards rewards interest stake =
      updateRewards' # rewards # interest # stake
    updateRewards' ::
      Term s (PNatRatio :--> PNatRatio :--> PNatural :--> PNatRatio)
    updateRewards' = phoistAcyclic $
      plam $ \rewards interest stake ->
        interest #* (toNatRatio stake #+ rewards)

stakeActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PBondedStakingDatum ->
  HRec
    '[ HField s "stakeAmount" PNatural
     , HField s "pubKeyHash" PPubKeyHash
     , HField s "maybeMintingAction" (PMaybeData PMintingAction)
     ] ->
  Term s PUnit
stakeActLogic txInfo params purpose datum act =
  unTermCont $ do
    -- Get the input being spent
    spentInput <-
      tcont . pletFields @PTxInInfoFields
        =<< getInput purpose txInfo.inputs
    let poolAddr :: Term s PAddress
        poolAddr = pfield @"address" # spentInput.resolved
    -- Check that input spent is not an asset UTXO (user cannot withdraw)
    doesNotConsumeAssetGuard datum
    -- Validate holder's signature
    guardC "stakeActLogic: tx not exclusively signed by the stake-holder" $
      signedOnlyBy txInfo.signatories act.pubKeyHash
    -- Check that amount is positive
    let stakeAmt :: Term s PNatural
        stakeAmt = pfromData act.stakeAmount
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
              txInfo.outputs
              $ getField @"data" txInfo
          )
    -- Check that the correct amount of the assetclass is locked
    bondedAsset <-
      tcont . pletFields @'["currencySymbol", "tokenName"] $
        params.bondedAssetClass
    guardC "stakeActLogic: amount deposited does not match redeemer's amount" $
      oneWith # (peq # bondedAsset.currencySymbol)
        # (peq # bondedAsset.tokenName)
        # (peq #$ pto $ pfromData act.stakeAmount)
        # assetOutput.value
    pure . pmatch act.maybeMintingAction $ \case
      -- If some minting action is provided, this is a new stake and inductive
      -- conditions must be checked
      PDJust mintAct -> unTermCont $ do
        -- Check that minted value is a list entry (minting policy is run)
        guardC "stakeActLogic: failure when checking minted value in minting tx" $
          hasListNft params.assocListCs txInfo.mint
        -- Check inductive conditions and business logic
        newStakeLogic
          txInfo
          params
          spentInput
          datum
          act.pubKeyHash
          act.stakeAmount
          $ pfield @"_0" # mintAct
      -- If no minting action is provided, this is a stake update
      PDNothing _ -> unTermCont $ do
        -- A list token should *not* be minted
        guardC
          "stakeActLogic: failure when checking minted value in non-minting \
          \ tx"
          $ pnot #$ hasListNft params.assocListCs txInfo.mint
        -- Check business logic
        updateStakeLogic
          txInfo
          params
          spentInput
          datum
          act.stakeAmount
          act.pubKeyHash
  where
    isAssetDatum :: Term s (PDatum :--> PBool)
    isAssetDatum = plam $ \dat' -> unTermCont $ do
      dat <-
        ptryFromUndata @PBondedStakingDatum
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
  PBondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PBondedStakingDatum ->
  Term s PNatural ->
  Term s PPubKeyHash ->
  TermCont s (Term s PUnit)
updateStakeLogic txInfo params spentInput datum stakeAmt holderPkh = do
  -- Construct some useful values for later
  stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
  stakeHolderTn <- pletC $ pcon $ PTokenName $ stakeHolderKey
  spentInputResolved <-
    tcont . pletFields @'["address", "value", "datumHash"] $ spentInput.resolved
  let poolAddr :: Term s PAddress
      poolAddr = spentInputResolved.address
      txInfoData = getField @"data" txInfo
  newEntryTok <- pletC $ passetClass # params.assocListCs # stakeHolderTn
  ---- FETCH DATUMS ----
  entry <- tcont . pletFields @PEntryFields =<< getEntryData datum
  newEntry <- getOutputEntry poolAddr newEntryTok txInfoData txInfo.outputs
  ---- BUSINESS LOGIC ----
  guardC "updateStakeLogic: spent entry's key does not match user's key" $
    entry.key #== stakeHolderKey
  guardC "updateStakeLogic: new entry does not have the stakeholder's key" $
    newEntry.key #== stakeHolderKey
  guardC "updateStakeLogic: incorrect update of newDeposit" $
    newEntry.newDeposit #== entry.newDeposit #+ stakeAmt
  guardC "updateStakeLogic: incorrect update of deposit" $
    newEntry.deposited #== entry.deposited #+ stakeAmt
  guardC "updateStakeLogic: update increases stake beyond allowed bounds" $
    pfromData params.minStake #<= newEntry.deposited
      #&& pfromData newEntry.deposited #<= params.maxStake
  guardC
    "updateStakeLogic: update should not change staked, rewards or next \
    \fields"
    $ entry.staked #== newEntry.staked
      #&& entry.rewards #== newEntry.rewards
      #&& entry.next #== newEntry.next

{- | This function checks all inductive conditions and makes all necessary
 business logic validation on the state/entry updates and new entries
-}
newStakeLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PBondedStakingDatum ->
  Term s PPubKeyHash ->
  Term s PNatural ->
  Term s PMintingAction ->
  TermCont s (Term s PUnit)
newStakeLogic txInfo params spentInput datum holderPkh stakeAmt mintAct =
  do
    -- Construct some useful values for later
    stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
    stakeHolderTn <- pletC $ pcon $ PTokenName $ stakeHolderKey
    spentInputResolved <-
      tcont . pletFields @'["address", "value", "datumHash"] $ spentInput.resolved
    let poolAddr :: Term s PAddress
        poolAddr = spentInputResolved.address
        stateTn :: Term s PTokenName
        stateTn = pconstant bondedStakingTokenName
    stateTok <- pletC $ passetClass # params.nftCs # stateTn
    newEntryTok <- pletC $ passetClass # params.assocListCs # stakeHolderTn
    txInfoData <- pletC $ getField @"data" txInfo
    pure . pmatch mintAct $ \case
      PMintHead stateOutRef -> unTermCont $ do
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
        (entryKey, _sizeLeft) <- getStateData datum
        -- Get datum for new list entry
        newEntry <- getOutputEntry poolAddr newEntryTok txInfoData txInfo.outputs
        ---- BUSINESS LOGIC ----
        newEntryGuard params newEntry stakeAmt stakeHolderKey
        ---- INDUCTIVE CONDITIONS ----
        -- Validate that spentOutRef is the state UTXO and matches redeemer
        guardC "newStakeLogic (mintHead): spent input is not the state UTXO" $
          spentInputResolved.value `hasStateToken`
            (params.nftCs, pconstant bondedStakingTokenName)
        guardC
          "newStakeLogic (mintHead): spent input does not match redeemer \
          \input"
          $ spentInput.outRef #== pfield @"_0" # stateOutRef
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
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PBondedStakingDatum ->
  HRec
  '[
    HField s "pubKeyHash" PPubKeyHash,
    HField s "burningAction" PBurningAction
  ] ->
  TermCont s (Term s PUnit)
withdrawActLogic
  txInfo
  params
  purpose
  datum
  act = do
  -- Get the input being spent
  spentInput <-
    tcont . pletFields @PTxInInfoFields
      =<< getInput purpose txInfo.inputs
  -- Validate holder's signature
  guardC "withdrawActLogic: tx not exclusively signed by the stake-holder" $
    signedOnlyBy txInfo.signatories act.pubKeyHash
  -- Get resulting output with staked assets and accrued rewards
  withdrawOutput <-
    getOutputSignedBy act.pubKeyHash txInfo.outputs
  -- Check business and inductive conditions depending on redeemer
  pure . pmatch (pfromData act.burningAction) $ \case
    PBurnHead outRefs' -> unTermCont $ do
      outRefs <- tcont . pletFields @'["state", "headEntry"] $ outRefs'
      -- We check all conditions when consuming the state UTXO
      --let withdrawHeadCheck =
      --      withdrawHeadActLogic
      --        spentInput act.pubKeyHash datum txInfo params outRefs.headEntry
      pure $ pif (spentInput.outRef #== outRefs.state)
              _
              punit
    PBurnOther entries' -> _

withdrawHeadActLogic ::
  forall (s :: S).
  HRec
  '[HField s "outRef" PTxOutRef,
    HField s "resolved" PTxOut
  ] ->
  Term s PPubKeyHash ->
  Term s PBondedStakingDatum ->
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  Term s PTxOutRef ->
  Term s PTxOutRef ->
  Term s PUnit
withdrawHeadActLogic spentInput holderPkh datum txInfo params stateOutRef headEntryOutRef =
  unTermCont $ do
  -- Construct some useful values for later
  stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
  stakeHolderTn <- pletC $ pcon $ PTokenName $ stakeHolderKey
  spentInputResolved <-
    tcont . pletFields @'["address", "value", "datumHash"] $ spentInput.resolved
  let poolAddr :: Term s PAddress
      poolAddr = spentInputResolved.address
      stateTn :: Term s PTokenName
      stateTn = pconstant bondedStakingTokenName
  stateTok <- pletC $ passetClass # params.nftCs # stateTn
  entryTok <- pletC $ passetClass # params.assocListCs # stakeHolderTn
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
  (entryKey, _sizeLeft) <- getStateData datum
  -- Get datum for head entry
  headEntry <- getInputEntry headEntryOutRef txInfoData txInfo.inputs
  ---- INDUCTIVE CONDITIONS ----
  -- Validate that spentOutRef is the state UTXO and matches redeemer
  guardC "withdrawHeadActLogic: spent input is not the state UTXO" $
    spentInputResolved.value `hasStateToken`
      (params.nftCs, pconstant bondedStakingTokenName)
  guardC
    "withdrawHeadActLogic: spent input does not match redeemer \
    \input"
    $ spentInput.outRef #== pdata stateOutRef
  -- Validate next state
  guardC
    "withdrawHeadActLogic: next pool state does not point to same location as \
    \burned entry"
    $ pdata nextEntryKey #== headEntry.next

closeActLogic ::
  forall (s :: S).
  Term s PTxInfo ->
  Term s PBondedPoolParams ->
  Term s PUnit
closeActLogic txInfo params = unTermCont $ do
  -- Retrieve fields from parameters
  txInfoF <-
    tcont $
      pletFields
        @'["signatories", "validRange"]
        txInfo
  paramsF <- tcont $ pletFields @'["admin"] params
  -- We check that the transaction was signed by the pool operator
  guardC "transaction not signed by admin" $
    signedBy txInfoF.signatories paramsF.admin
  -- We check that the transaction occurs during the closing period
  -- We don't validate this for the demo, otherwise testing becomes
  -- too difficult
  -- period <- pure $ getPeriod # txInfoF.validRange # params
  -- guardC "admin deposit not done in closing period" $
  --  isClosingPeriod period
  pure punit
  where
    _isClosingPeriod :: Term s PPeriod -> Term s PBool
    _isClosingPeriod period = pmatch period $ \case
      ClosingPeriod -> pconstant True
      _ -> pconstant False

-- Helper functions for the different logics

-- Retrieves state fields from staking datum
getStateData ::
  forall (s :: S).
  Term s PBondedStakingDatum ->
  TermCont s (Term s (PMaybeData PByteString), Term s PNatural)
getStateData datum = do
  record <- tcont . pletFields @["_0", "_1"] . pmatch datum $ \case
    PStateDatum record -> record
    _ -> ptraceError "getStateData: datum is not PStateDatum"
  pure (pfromData record._0, pfromData record._1)

-- Retrieves entry fields from staking datum
getEntryData ::
  forall (s :: S).
  Term s PBondedStakingDatum ->
  TermCont s (Term s PEntry)
getEntryData datum =
  pure . pmatch datum $ \case
    PEntryDatum entry -> pfield @"_0" # entry
    _ -> ptraceError "getEntryDatum: datum is not PEntryDatum"

-- Returns true if it points to the given PKH
pointsTo ::
  forall (s :: S).
  Term s (PMaybeData PByteString) ->
  Term s PByteString ->
  Term s PBool
pointsTo entryKey tn = pointsTo' # entryKey # tn
  where
    pointsTo' :: Term s (PMaybeData PByteString :--> PByteString :--> PBool)
    pointsTo' = phoistAcyclic $
      plam $ \e t ->
        pmatch e $ \case
          PDJust t' -> pfield @"_0" # t' #== t
          PDNothing _ -> pfalse

-- Returns false if it points nowhere
pointsNowhere ::
  forall (s :: S).
  Term s (PMaybeData PByteString) ->
  Term s PBool
pointsNowhere x = pointsNowhere' # x
  where
    pointsNowhere' :: Term s (PMaybeData PByteString :--> PBool)
    pointsNowhere' = phoistAcyclic $
      plam . flip pmatch $ \case
        PDJust _ -> pfalse
        PDNothing _ -> ptrue

-- Gets the CO and datum that satisfy the given datum predicate.
-- It fails if no CO is found, or no CO satisfies the predicate, or too many COs
-- are found
getCoWithDatum ::
  forall (s :: S).
  Term s PAddress ->
  Term s (PDatum :--> PBool) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum))) ->
  TermCont s (Term s PTxOut, Term s PDatum)
getCoWithDatum poolAddr pred outputs datums = do
  -- Filter COs with address and datum from outputs
  cos <- pletC . flip pmapMaybe outputs . plam $
    \output -> unTermCont $ do
      outputF <- tcont $ pletFields @'["address", "datumHash"] output
      pure $
        pif
          (pnot #$ pdata outputF.address #== pdata poolAddr)
          -- Address does not match, ignore output
          (pcon PNothing)
          $ pmatch outputF.datumHash $ \case
            -- Output does not have a datum, fail (pool outputs always
            -- should)
            PDNothing _ ->
              ptraceError "getCoWithDatum: found CO without a datum"
            PDJust datHash -> unTermCont $ do
              datum <- getDatum (pfield @"_0" # datHash) datums
              pure $
                pif
                  (pred # datum)
                  ( pcon . PJust . pbuiltinPairFromTuple . pdata $
                      ptuple # output # pdata datum
                  )
                  -- Datum does not satisfy predicate, ignore output
                  (pcon PNothing)
  -- Make sure it's the only output
  ppairData . pmatch cos $ \case
    PNil ->
      ptraceError
        "getCoWithDatum: found more than one CO with given \
        \ datum"
    PCons x xs -> pmatch xs $ \case
      PCons _ _ ->
        ptraceError
          "getCoWithDatum: found more than one CO with\
          \ given datum"
      PNil -> pfromData x

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
  entry <- flip pfind inputs $ plam $ \input ->
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
  PBondedPoolParamsHRec s ->
  PEntryHRec s ->
  Term s PNatural ->
  Term s PByteString ->
  TermCont s (Term s PUnit)
newEntryGuard params newEntry stakeAmt stakeHolderKey = do
  guardC
    "newEntryGuard: incorrect init. of newDeposit and deposit fields \
    \in first stake"
    $ newEntry.newDeposit #== stakeAmt
      #&& newEntry.deposited #== stakeAmt
  guardC "newEntryGuard: new entry does not have the stakeholder's key" $
    newEntry.key #== stakeHolderKey
  guardC "newEntryGuard: new entry's stake not within stake bounds" $
    pfromData params.minStake #<= newEntry.deposited
      #&& pfromData newEntry.deposited #<= params.maxStake
  guardC
    "newEntryGuard: new entry's staked and rewards fields not \
    \initialized to zero"
    $ newEntry.staked #== natZero
      #&& newEntry.rewards #== ratZero

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
      #&& e1.newDeposit #== e2.newDeposit
      #&& e1.staked #== e2.staked
      #&& e1.rewards #== e2.rewards

natZero :: Term s PNatural
natZero = pconstant $ Natural 0

ratZero :: Term s PNatRatio
ratZero = pconstant . NatRatio $ 0
