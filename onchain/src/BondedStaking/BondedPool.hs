{-# LANGUAGE UndecidableInstances #-}

module BondedStaking.BondedPool (
  pbondedPoolValidator,
  pbondedPoolValidatorUntyped,
) where

import BondedStaking.PTypes (
  PBondedPoolParams,
  PBondedPoolParamsFields,
  PBondedPoolParamsHRec,
  PBondedStakingAction (
    PAdminAct,
    PCloseAct,
    PStakeAct,
    PWithdrawAct
  ),
  PBondedStakingDatum (PAssetDatum, PEntryDatum, PStateDatum),
 )

import Plutarch.Api.V1 (
  PAddress,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose,
  PTxInfo,
  PMaybeData (PDJust, PDNothing),
  PValue,
  PCurrencySymbol,PTokenName (PTokenName)
 )
import Plutarch.Unsafe (punsafeCoerce)

import PNatural (
  PNatural,
 )
import PTypes (
  HField,
  PMintingAction(PMintHead, PMintInBetween, PMintEnd),
  PPeriod (BondingPeriod, ClosingPeriod),
  PAssetClass,

  PTxInfoFields,
  PTxInfoHRec,
  PTxInInfoHRec,
  PTxInInfoFields,
  --bondingPeriod,
  --depositWithdrawPeriod,
  --onlyWithdrawPeriod,
  passetClass,
 )

--import PInterval(
--  getBondedPeriod
--  )

import Utils (
  getContinuingOutputWithNFT,
  getDatum,
  getDatumHash,
  getInput,
  guardC,
  parseStakingDatum,
  pconstantC,
  pletC,
  pmatchC,
  ptryFromUndata,
  punit,
  signedBy,
  signedOnlyBy,
  ptrue,
  pfalse,
  oneWith,
  peq,
  pconst,
 )

import GHC.Records (getField)
import SingularityNet.Settings (bondedStakingTokenName)
import InductiveLogic (hasListNft, consumesStateUtxoGuard)
import Plutarch.DataRepr (HRec)
import Plutarch.DataRepr.Internal.Field (Labeled)
import Plutarch.Crypto (pblake2b_256)

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
          --guardC "pbondedPoolValidator: wrong period for PAdminAct redeemer" $
          --  getBondedPeriod # txInfoF.validRange # params #== bondingPeriod
          pure $ adminActLogic txInfoF paramsF ctxF.purpose dat $
            pfield @"_0" # n
        PStakeAct act -> unTermCont $ do
          --guardC "pbondedPoolValidator: wrong period for PStakeAct \
          -- \redeemer" $
          --  getBondedPeriod # txInfoF.validRange # params
          --     #== depositWithdrawPeriod
          pure $
            pletFields @'["stakeAmount", "pubKeyHash", "maybeMintingAction"]
            act $ \actF -> stakeActLogic
              txInfoF
              paramsF
              ctxF.purpose
              dat
              actF
        PWithdrawAct _act -> unTermCont $ do
          --period <- getBondedPeriod # txInfoF.validRange # params
          --guardC "pbondedPoolValidator: wrong period for PWithdrawAct \
          --  \redeemer" $
          --  period #== depositWithdrawPeriod #|| onlyWithdrawPeriod
          guardC "pbondedPoolValidator: a token should be burned when using \
            \ PWithdrawAct" $
            isBurningEntry txInfoF.mint paramsF.assocListCs
          pure withdrawActLogic
        PCloseAct _ -> unTermCont $ do
          -- guardC "pbondedPoolValidator: wrong period for PcloseAct redeemer" $
          --   getBondedPeriod # txInfoF.validRange # params #== closingPeriod
          pure $ closeActLogic ctxF.txInfo params
  where isBurningEntry :: forall (s :: S) .
          Term s PValue -> Term s PCurrencySymbol -> Term s PBool
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

-- The pool operator calculates the new available size left
-- TODO: Besides the logic related to updating the entries, there should also
-- be a check that makes sure the admin is not _reducing_ the stakes or the
-- rewards
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
  input <- getInput purpose txInfo.inputs
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
        let cs = params.nftCs
            tn = pconstant bondedStakingTokenName
            ac = passetClass # cs # tn
        -- We retrieve the continuing output's datum
        coOutput <- getContinuingOutputWithNFT inputAddress ac txInfo.outputs
        coOutputDatumHash <- getDatumHash coOutput
        coOutputDatum <- getDatum coOutputDatumHash $ getField @"data" txInfo
        coOutputStakingDatum <- parseStakingDatum @PBondedStakingDatum coOutputDatum
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
        let _cs = params.assocListCs
            _tn = pconstant bondedStakingTokenName
            _ac = passetClass # _cs # _tn
        -- TODO: Verify that most fields are kept intact, that size is updated
        -- and that interests are calculated correctly
        pconstantC ()
      PAssetDatum _ ->
        ptraceError
          "adminActLogic: update failed because a wrong \
          \datum constructor was provided"
  where
    __isBondingPeriod :: Term s PPeriod -> Term s PBool
    __isBondingPeriod period = pmatch period $ \case
      BondingPeriod -> pconstant True
      _ -> pconstant False

stakeActLogic :: forall (s :: S).
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PBondedStakingDatum ->
  HRec '[
    HField s "stakeAmount" PNatural,
    HField s "pubKeyHash" PPubKeyHash,
    HField s "maybeMintingAction" (PMaybeData PMintingAction)
  ] ->
  Term s PUnit
stakeActLogic txInfo params purpose datum act =
  unTermCont $ do
  -- Get the input being spent
  spentInput <- tcont . pletFields @PTxInInfoFields =<<
    getInput purpose txInfo.inputs
  -- Check holder's signature
  guardC "stakeActLogic: tx not exclusively signed by the stake-holder" $
    signedOnlyBy txInfo.signatories act.pubKeyHash
  pure $ pmatch act.maybeMintingAction $ \case
    -- If some minting action is provided, this is a new stake and inductive
    -- conditions must be checked
    PDJust mintAct -> unTermCont $ do
      guardC "stakeActLogic: failure when checking minted value in minting tx" $
        hasListNft params.assocListCs txInfo.mint
      -- Check inductive conditions and business logic
      newStakeLogic txInfo params spentInput datum act.stakeAmount act.pubKeyHash $
        pfield @"_0" # mintAct
    -- If no minting action is provided, this is a stake update
    PDNothing _ -> unTermCont $ do
      -- A list token should *not* be minted
      guardC "stakeActLogic: failure when checking minted value in non-minting \
        \ tx" $
        pnot #$ hasListNft params.assocListCs txInfo.mint
      -- Check business logic
      updateStakeLogic txInfo params spentInput act.stakeAmount act.pubKeyHash


-- TODO
updateStakeLogic :: forall (s :: S) .
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PNatural ->
  Term s PPubKeyHash ->
  TermCont s (Term s PUnit)
updateStakeLogic txInfo params spentInput stakeAmt holderPkh = do
  pure punit

newStakeLogic :: forall (s :: S) .
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PBondedStakingDatum ->
  Term s PNatural ->
  Term s PPubKeyHash ->
  Term s PMintingAction ->
  TermCont s (Term s PUnit)
newStakeLogic txInfo params spentInput stateDatum stakeAmt holderPkh mintAct = do
  pure . pmatch mintAct $ \case
    PMintHead _ -> unTermCont $ do
      -- Validate that spentOutRef is the state UTXO
      consumesStateUtxoGuard
        spentInput.outRef
        txInfo.inputs
        params.nftCs
        $ pconstant bondedStakingTokenName
      -- Get datum for next state
      nextStateTxOut <-
        getContinuingOutputWithNFT poolAddress stateTok txInfo.outputs
      nextStateHash <- getDatumHash nextStateTxOut
      (nextEntryTn, nextSizeLeft) <-
        getStateData =<<
        parseStakingDatum =<<
        getDatum nextStateHash (getField @"data" txInfo)
      -- Get datum for current state
      (entryTn, sizeLeft) <- getStateData stateDatum
      -- Get datum for new list entry
      newHeadStateTxOut <-
        getContinuingOutputWithNFT poolAddress listTok txInfo.outputs
      newHeadHash <- getDatumHash nextStateTxOut
      -- newHeadDatum <-
      --   getEntryData =<<
      --   parseEntryDatum =<<
      --   getDatum newHeadHash (getField @"data" txInfo)
      -- Validate list insertion and state update
      pure . pmatch entryTn $ \case
        -- We are shifting the current head forwards
        PDJust entryTn' -> undefined
        -- This is the first stake of the pool
        PDNothing _ -> unTermCont $ do
          pure punit
    PMintInBetween outRefs ->
      punit
    PMintEnd listEndOutRef ->
      punit
  where poolAddress :: Term s PAddress
        poolAddress = pfield @"address" # spentInput.resolved
        stateTok :: Term s PAssetClass
        stateTok = passetClass # params.nftCs # pconstant bondedStakingTokenName
        listTok :: Term s PAssetClass
        listTok = passetClass # params.assocListCs #$
          pcon . PTokenName $ pblake2b_256 # pto holderPkh


withdrawActLogic :: forall (s :: S). Term s PUnit
withdrawActLogic = pconstant ()

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
  pconstantC ()
  where
    _isClosingPeriod :: Term s PPeriod -> Term s PBool
    _isClosingPeriod period = pmatch period $ \case
      ClosingPeriod -> pconstant True
      _ -> pconstant False

-- Helper functions for the different logics
getStateData :: forall (s :: S).
  Term s PBondedStakingDatum ->
  TermCont s (Term s (PMaybeData PByteString), Term s PNatural)
getStateData datum = do
  record <- tcont . pletFields @["_0", "_1"] . pmatch datum $ \case
    PStateDatum record -> record
    _ -> ptraceError "getStateData: datum is not PStateDatum"
  pure (pfromData record._0, pfromData record._1)

getEntryData :: forall (s :: S).
  Term s PBondedStakingDatum ->
  TermCont s (Term s (PMaybeData PByteString), Term s PNatural)
getEntryData datum = do
  record <- tcont . pletFields @["_0", "_1"] . pmatch datum $ \case
    PStateDatum record -> record
    _ -> ptraceError "getStateData: datum is not PStateDatum"
  pure (pfromData record._0, pfromData record._1)
