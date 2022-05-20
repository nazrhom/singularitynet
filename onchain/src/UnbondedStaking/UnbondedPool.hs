{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module UnbondedStaking.UnbondedPool (
  punbondedPoolValidator,
  punbondedPoolValidatorUntyped,
) where

import SingularityNet.Natural (
  Natural (Natural),

  NatRatio (NatRatio),
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

import PNatural (
  PNatRatio,
  PNatural,

  PNonNegative ((#+), (#*), (#-)),
 )

import SingularityNet.Settings (unbondedStakingTokenName)
import PTypes (
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

import Data.Ratio (
  (%),
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
          PStakeAct _pair -> stakeActLogic
          PWithdrawAct _pkh -> withdrawActLogic
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
        PStateDatum _ ->
          ptraceError
            "adminActLogic: update failed because a wrong \
            \datum constructor was provided"
        PEntryDatum oldEntryRecord -> unTermCont $ do
          -- Retrieve fields from oldEntry
          oldEntry <- pletC $ pfield @"_0" # oldEntryRecord
          oldEntryF <-
            tcont $
              pletFields
                @'[ "key"
                  , "deposited"
                  , "newDeposit"
                  , "rewards"
                  , "totalRewards"
                  , "totalDeposited"
                  , "open"
                  , "next"
                  ]
                oldEntry
          -- Ensure pool is open
          guardC
            "adminActLogic: update failed because pool is not open"
            oldEntryF.open
          -- Validate new Entry datum
          -- We obtain the asset class of the NFT
          let cs = pfield @"assocListCs" # params
              tn = pconstant unbondedStakingTokenName
              ac = passetClass # cs # tn
          -- We retrieve the continuing output's datum
          coOutput <- getContinuingOutputWithNFT inputAddress ac txInfoF.outputs
          coOutputDatumHash <- getDatumHash coOutput
          coOutputDatum <- getDatum coOutputDatumHash $ getField @"data" txInfoF
          coOutputStakingDatum <- parseStakingDatum coOutputDatum
          -- Retrieve fields from new Entry
          PEntryDatum newEntryRecord <- pmatchC coOutputStakingDatum
          newEntry <- pletC $ pfield @"_0" # newEntryRecord
          newEntryF <-
            tcont $
              pletFields
                @'[ "key"
                  , "deposited"
                  , "newDeposit"
                  , "rewards"
                  , "totalRewards"
                  , "totalDeposited"
                  , "open"
                  , "next"
                  ]
                newEntry
          -- Check updated values
          guardC
            "adminActLogic: update failed because entry field 'key' \
            \is changed"
            $ oldEntryF.key #== newEntryF.key
          guardC
            "adminActLogic: update failed because entry field 'deposited' \
            \is changed"
            $ oldEntryF.deposited #== newEntryF.deposited
          guardC
            "adminActLogic: update failed because entry field 'newDeposit' \
            \is not zero"
            $ newEntryF.newDeposit #==
              pconstant @PNatural (Natural $ fromInteger 0)
          guardC
            "adminActLogic: update failed because entry field 'rewards' \
            \is not updatedRewards"
            $ newEntryF.rewards #==
              updatedRewards
                oldEntryF.rewards
                tRewards
                oldEntryF.deposited
                oldEntryF.newDeposit
                tDeposited
          guardC
            "adminActLogic: update failed because entry field 'totalRewards' \
            \is not newTotalRewards"
            $ newEntryF.totalRewards #== tRewards
          guardC
            "adminActLogic: update failed because entry field \
            \'totalDeposited' is not newTotalDeposited"
            $ newEntryF.totalDeposited #== tDeposited
          guardC
            "adminActLogic: update failed because entry field 'open' \
            \is changed"
            $ oldEntryF.open #== newEntryF.open
          guardC
            "adminActLogic: update failed because entry field 'next' \
            \is changed"
            $ oldEntryF.next #== newEntryF.next

          pconstantC ()
        PAssetDatum _ ->
          ptraceError
            "adminActLogic: update failed because a wrong \
            \datum constructor was provided"

  where
    updatedRewards ::
      forall (s :: S).
      Term s PNatRatio ->
      Term s PNatural ->
      Term s PNatural ->
      Term s PNatural ->
      Term s PNatural ->
      Term s PNatRatio
    updatedRewards _ _ _ _ _ = pconstant @PNatRatio (NatRatio (fromInteger 1 % fromInteger 1))
    updatedRewards r tr d nd td = r #+ f
      where

        f = tr #* ((d - nd #+ r) / td)

-- instance POrd PNatRatio where
--   a #<= b = P.do
--     a' <- plet $ pto a
--     b' <- plet $ pto b
--     let n1 = pfstData # a'
--         d1 = psndData # a'
--         n2 = pfstData # b'
--         d2 = psndData # b'
--     n1 * d2 #<= n2 * d1
--   a #< b = P.do
--     a' <- plet $ pto a
--     b' <- plet $ pto b
--     let n1 = pfstData # a'
--         d1 = psndData # a'
--         n2 = pfstData # b'
--         d2 = psndData # b'
--     n1 * d2 #< n2 * d1

-- pfstData ::
--   forall (s :: S) (a :: PType) (b :: PType).
--   PIsData a =>
--   Term s (PBuiltinPair (PAsData a) b :--> a)
-- pfstData = phoistAcyclic $ plam $ \x -> pfromData $ pfstBuiltin # x

-- psndData ::
--   forall (s :: S) (a :: PType) (b :: PType).
--   PIsData b =>
--   Term s (PBuiltinPair a (PAsData b) :--> b)
-- psndData = phoistAcyclic $ plam $ \x -> pfromData $ psndBuiltin # x

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
