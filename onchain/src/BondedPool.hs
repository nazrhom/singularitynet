{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module BondedPool (
  pbondedPoolValidator,
  pbondedPoolValidatorUntyped,
) where

import Plutarch.Api.V1 (
  PAddress,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose,
  PTxInfo,
  PMaybeData (PDJust, PDNothing),
  PValue,
  PCurrencySymbol,PTokenName (PTokenName), PTxOut, PTuple, PDatumHash, ptuple
 )
import Plutarch.Builtin (pforgetData)
import Plutarch.Unsafe (punsafeCoerce)

import PNatural (
  PNatural,
 )
import PTypes (
  PBondedPoolParams,
  PMintingAction(PMintHead, PMintInBetween, PMintEnd),
  PBondedStakingAction (
    PAdminAct,
    PCloseAct,
    PStakeAct,
    PWithdrawAct
  ),
  PBondedStakingDatum (PAssetDatum, PEntryDatum, PStateDatum),
  PPeriod (BondingPeriod, ClosingPeriod),
  PAssetClass,
  PEntry,
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
  pmapMaybe,
  pfstData,
  psndData,
  PTxInfoFields,
  PTxInfoHRec,
  PBondedPoolParamsFields,
  PBondedPoolParamsHRec,
  PTxInInfoHRec,
  PTxInInfoFields,
  HField,
  PEntryFields,
 )

import GHC.Records (getField)
import Plutarch.Api.V1.Scripts (PDatum)
import SingularityNet.Settings (bondedStakingTokenName)
import InductiveLogic (hasListNft, consumesStateUtxoGuard, doesNotConsumeAssetGuard)
import Plutarch.DataRepr (HRec)
import Plutarch.Crypto (pblake2b_256)
import SingularityNet.Natural (Natural(Natural))
import Plutarch.Api.V1.Tuple (pbuiltinPairFromTuple)

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
          pure . pletFields
            @'["stakeAmount", "pubKeyHash", "maybeMintingAction"]
            act
            $ \actF -> stakeActLogic
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
  let poolAddr :: Term s PAddress
      poolAddr = pfield @"address" # spentInput.resolved
  -- Check that input spent is not an asset UTXO (user cannot withdraw)
  doesNotConsumeAssetGuard datum
  -- Validate holder's signature
  guardC "stakeActLogic: tx not exclusively signed by the stake-holder" $
    signedOnlyBy txInfo.signatories act.pubKeyHash
  -- Check that amount is positive and within bounds
  let stakeAmt :: Term s PNatural
      stakeAmt = pfromData act.stakeAmount
  guardC "stakeActLogic: stake amount is not positive or within bounds" $
    zero #<= stakeAmt
    #&& params.minStake #<= stakeAmt
    #&& stakeAmt #<= params.maxStake
  -- Get asset output of the transaction (new locked stake)
  assetOutput <-
    (tcont . pletFields @'["value"])
    =<< fmap fst
    (getCoWithDatum
          poolAddr
          isAssetDatum
          txInfo.outputs
          $ getField @"data" txInfo)
  -- Check that the correct amount of the assetclass is locked
  bondedAsset <- tcont . pletFields @'["currencySymbol", "tokenName"] $
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
      -- Check that minted value is a list entry
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
      guardC "stakeActLogic: failure when checking minted value in non-minting \
             \ tx" $
        pnot #$ hasListNft params.assocListCs txInfo.mint
      -- Check business logic
      updateStakeLogic txInfo params spentInput act.stakeAmount act.pubKeyHash
  where isAssetDatum :: Term s (PDatum :--> PBool)
        isAssetDatum = plam $ \dat' -> unTermCont $ do
          dat <- ptryFromUndata @PBondedStakingDatum
               . pforgetData
               . pdata
               $ dat'
          pure . pmatch dat $ \case
            PAssetDatum _ -> ptrue
            _ -> pfalse
    
-- TODO
updateStakeLogic :: forall (s :: S) .
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PNatural ->
  Term s PPubKeyHash ->
  TermCont s (Term s PUnit)
updateStakeLogic _txInfo _params _spentInput _stakeAmt _holderPkh = do
  pure punit

-- | This function checks all inductive condition and returns the old and new
-- entry for further business logic checks
newStakeLogic :: forall (s :: S) .
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PBondedStakingDatum ->
  Term s PPubKeyHash ->
  Term s PNatural ->
  Term s PMintingAction ->
  TermCont s (Term s PUnit)
newStakeLogic txInfo params spentInput stateDatum holderPkh stakeAmt mintAct =
  do
  pure . pmatch mintAct $ \case
    PMintHead _ -> unTermCont $ do
      -- Validate that spentOutRef is the state UTXO
      consumesStateUtxoGuard
        spentInput.outRef
        txInfo.inputs
        params.nftCs
        $ pconstant bondedStakingTokenName
      -- Construct some useful values for later
      stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
      let poolAddress :: Term s PAddress
          poolAddress = pfield @"address" # spentInput.resolved
          stateTok :: Term s PAssetClass
          stateTok = passetClass # params.nftCs # pconstant bondedStakingTokenName 
          listTok :: Term s PAssetClass
          listTok = passetClass # params.assocListCs #$
            pcon $ PTokenName $ stakeHolderKey
      ---- FETCH DATUMS ----
      -- Get datum for next state
      nextStateTxOut <- 
        getContinuingOutputWithNFT poolAddress stateTok txInfo.outputs
      nextStateHash <- getDatumHash nextStateTxOut
      (nextEntryKey, _nextSizeLeft) <-
        getStateData =<<
        parseStakingDatum =<<
        getDatum nextStateHash (getField @"data" txInfo)
      -- Get datum for current state
      (entryKey, _sizeLeft) <- getStateData stateDatum
      -- Get datum for new list entry
      newEntryTxOut <-
        getContinuingOutputWithNFT poolAddress listTok txInfo.outputs
      newEntryHash <- getDatumHash newEntryTxOut 
      newEntry <-
        tcont . pletFields @PEntryFields =<<
        getEntryData =<<
        parseStakingDatum =<<
        getDatum newEntryHash (getField @"data" txInfo)
      -- Check business logic conditions in entry
      guardC "newStakeLogic: incorrect init. of newDeposit field in first\
             \ stake" $
        newEntry.newDeposit #== stakeAmt
      guardC "newStakeLogic: new entry does not have the stakeholder's key" $
        newEntry.key #== stakeHolderKey
      -- Validate list insertion and state update
      pure . pmatch entryKey $ \case
        -- We are shifting the current head forwards
        PDJust _entryKey' -> undefined
        -- This is the first stake of the pool
        PDNothing _ -> unTermCont $ do
          -- The state now should point to the new entry
          guardC "newStakeLogic: pool does not point to first entry" $
            nextEntryKey `pointsTo` newEntry.key
          pure punit
    PMintInBetween _outRefs ->
      punit
    PMintEnd _listEndOutRef ->
      punit

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
  pure punit
  where
    _isClosingPeriod :: Term s PPeriod -> Term s PBool
    _isClosingPeriod period = pmatch period $ \case
      ClosingPeriod -> pconstant True
      _ -> pconstant False

-- Helper functions for the different logics
parseStakingDatum ::
  forall (s :: S).
  Term s PDatum ->
  TermCont s (Term s PBondedStakingDatum)
parseStakingDatum = ptryFromUndata @PBondedStakingDatum . pforgetData . pdata

-- Retrieves state fields from staking datum
getStateData :: forall (s :: S).
  Term s PBondedStakingDatum ->
  TermCont s (Term s (PMaybeData PByteString), Term s PNatural)
getStateData datum = do
  record <- tcont . pletFields @["_0", "_1"] . pmatch datum $ \case
    PStateDatum record -> record
    _ -> ptraceError "getStateData: datum is not PStateDatum"
  pure (pfromData record._0, pfromData record._1)

-- Retrieves entry fields from staking datum
getEntryData :: forall (s :: S).
  Term s PBondedStakingDatum ->
  TermCont s (Term s PEntry)
getEntryData datum =
  pure . pmatch datum $ \case
    PEntryDatum entry -> pfield @"_0" # entry
    _ -> ptraceError "getEntryDatum: datum is not PEntryDatum"
    
-- Returns true if it points to the given PKH
pointsTo :: forall (s :: S).
  Term s (PMaybeData PByteString) ->
  Term s PByteString ->
  Term s PBool
pointsTo entryKey tn = pointsTo' # entryKey # tn
  where pointsTo' :: Term s (PMaybeData PByteString :--> PByteString :--> PBool)
        pointsTo' = phoistAcyclic $ plam $ \e t ->
          pmatch e $ \case
            PDJust t' -> pfield @"_0" # t' #== t
            PDNothing _ -> pfalse
            
-- Gets the CO and datum that satisfy the given datum predicate.
-- It fails if no CO is found, or no CO satisfies the predicate, or too many COs
-- are found
getCoWithDatum :: forall (s :: S).
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
            pure $ pif (pnot #$ pdata outputF.address #== pdata poolAddr)
              -- Address does not match, ignore output
              (pcon PNothing)
              $ pmatch outputF.datumHash $ \case
                -- Output does not have a datum, fail (pool outputs always
                -- should)
                PDNothing _ ->
                  ptraceError "getCoWithDatum: found CO without a datum"
                PDJust datHash -> unTermCont $ do
                  datum <- getDatum (pfield @"_0" # datHash) datums
                  pure $ pif (pred # datum)
                         (pcon . PJust . pbuiltinPairFromTuple . pdata $
                            ptuple # output # pdata datum)
                         -- Datum does not satisfy predicate, ignore output
                         (pcon PNothing)
  -- Make sure it's the only output
  co <- pletC . pmatch cos $ \case
        PNil -> ptraceError "getCoWithDatum: found more than one CO with given \
                            \ datum"
        PCons x xs -> pmatch xs $ \case
          PCons _ _ -> ptraceError "getCoWithDatum: found more than one CO with\
                                   \ given datum"
          PNil -> pfromData x

  pure (pfstData co, psndData co)

zero :: Term s PNatural
zero = pconstant $ Natural 0
