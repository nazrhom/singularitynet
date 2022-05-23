module ListNFT (
  plistNFTPolicy,
  plistNFTPolicyUntyped,
) where

import Plutarch.Api.V1 (
  PCurrencySymbol,
  PPubKeyHash,
  PScriptContext,
  PTokenName (PTokenName),
  PValue,
  PTxOutRef, PTxInInfo (PTxInInfo)
 )
import Plutarch.Api.V1.Scripts ()
import Plutarch.Unsafe (punsafeCoerce)

import PTypes (PMintingAction (..))
import Plutarch.Crypto (pblake2b_256)
import Utils (
  getCs,
  guardC,
  oneOf,
  oneOfWith,
  pconstantC,
  peq,
  pletC,
  ptryFromUndata,
  ptrue
 )

{-
    This module implements the minting policy for the NFTs used as entries for
    the user's stakes in the on-chain association list.

    Some level of validation is performed by this policy (at least the bare
    minimum related to any NFT), as well as some other validation specific
    to each user action. Specifically, most invariants related to maintaining
    the linked list here.

    Due to how the system is designed, this minting policy is only run when
    a user stakes *for the first time* (minting a new NFT) or when it withdraws
    their stake (burning an NFT). Invariants related to the amount staked,
    global or local limits, and other things are mostly delegated to the pool
    validator and not treated here.
-}

-- TODO: Inductive conditions related to staking and withdrawing not implemented
plistNFTPolicy ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol
        :--> PMintingAction
        :--> PScriptContext
        :--> PUnit
    )
plistNFTPolicy = plam $ \nftCs mintAct ctx' -> unTermCont $ do
  -- This CurrencySymbol is only used for parametrization
  _cs <- pletC nftCs
  ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  -- Own CurrencySymbol
  cs <- getCs ctx.purpose
  txInfo <- tcont $ pletFields @'["signatories", "mint", "inputs"] ctx.txInfo
  -- Get a *single* signatory or fail
  signatory <- getSignatory txInfo.signatories
  -- Calculate TokenName based on PubKeyHash
  let tn :: Term s PTokenName
      tn = pcon . PTokenName $ pblake2b_256 # pto signatory
  -- Functions for validating the minted value
  let guardMint :: TermCont s (Term s PUnit)
      guardMint = guardC "pliftNFTPolicy: failed when checking minted value" $
          checkMint cs tn txInfo.mint
      guardBurn :: TermCont s (Term s PUnit)
      guardBurn = guardC "pliftNFTPolicy: failed when checking minted value" $
          checkBurn cs tn txInfo.mint
  -- Dispatch to appropiate handler based on redeemer
  pure $
    pmatch mintAct $ \case
      PStakeHead poolStateOutRef' -> unTermCont $ do
        let outRef :: Term s PTxOutRef
            outRef = pfromData $ pfield @"_0" # poolStateOutRef'
        guardMint
        -- One input (outRef) should have the state NFT. No list NFTs allowed!
        guardC "pliftNFTPolicy: failed when checking inputs for PStakeHead" $
          pall # stakeHeadCheck # txInfo.inputs
      PStakeInBetween entries -> unTermCont $ do
        guardMint
      PStakeEnd lastEntry' -> unTermCont $ do
        guardMint
      PWithdrawHead poolState' -> unTermCont $ do
        guardBurn
      PWithdrawOther prevEntry' -> unTermCont $ do
        guardBurn
  where stakeHeadCheck :: Term s (PAsData PTxInInfo :--> PBool)
        stakeHeadCheck = plam $ \input -> unTermCont $ do
          txOut <- pletC $ pfromData $ pfield @"resolved" # input
          txOutF <- tcont $ pletFields @["address", "value"] txOut
          -- TODO
          pure ptrue

plistNFTPolicyUntyped ::
  forall (s :: S). Term s (PData :--> PData :--> PData :--> PUnit)
plistNFTPolicyUntyped = plam $ \nftCs mintAct ctx ->
  plistNFTPolicy # unTermCont (ptryFromUndata nftCs)
    # unTermCont (ptryFromUndata mintAct)
    # punsafeCoerce ctx

-- Get the single signatory of the transaction or fail
getSignatory ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  TermCont s (Term s PPubKeyHash)
getSignatory ls = pure . pmatch ls $ \case
  PCons pkh ps ->
    pif
      (pnull # ps)
      (pfromData pkh)
      (ptraceError "getSignatory: transaction has more than one signatory")
  PNil -> ptraceError "getSignatory: empty list of signatories"

-- Check that the token was minted once
checkMint ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  Term s PBool
checkMint cs tn mintVal = checkMint' # cs # tn # mintVal
  where checkMint' ::
          Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PBool)
        checkMint' = phoistAcyclic $ plam $ \cs tn mintVal ->
            oneOf # cs # tn # mintVal

-- Check that the token was burnt once
checkBurn ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  Term s PBool
checkBurn cs tn mintVal = checkBurn' # cs # tn # mintVal
  where checkBurn' ::
          Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PBool)
        checkBurn' = phoistAcyclic $ plam $ \cs tn mintVal ->
          oneOfWith
            # (peq # cs)
            # (peq # tn)
            # (plam $ \n -> n #== -1)
            # mintVal
