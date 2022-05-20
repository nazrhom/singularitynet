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
 )
import Plutarch.Api.V1.Scripts ()
import Plutarch.Unsafe (punsafeCoerce)

import PTypes (PMintingAction (PStake, PWithdraw))
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
  txInfo <- tcont $ pletFields @'["signatories", "mint"] ctx.txInfo
  -- Get a *single* signatory or fail
  signatory <- getSignatory txInfo.signatories
  -- Calculate TokenName based on PubKeyHash
  let tn :: Term s PTokenName
      tn = pcon . PTokenName $ pblake2b_256 # pto signatory
  -- Dispatch to appropiate handler based on redeemer
  pure $
    pmatch mintAct $ \case
      PStake _ -> stakeActLogic cs tn txInfo.mint
      PWithdraw _ -> withdrawActLogic cs tn txInfo.mint

plistNFTPolicyUntyped ::
  forall (s :: S). Term s (PData :--> PData :--> PData :--> PUnit)
plistNFTPolicyUntyped = plam $ \nftCs mintAct ctx ->
  plistNFTPolicy # unTermCont (ptryFromUndata nftCs)
    # unTermCont (ptryFromUndata mintAct)
    # punsafeCoerce ctx

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

stakeActLogic ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  Term s PUnit
stakeActLogic cs tn mintVal = unTermCont $ do
  -- Check that the token was minted once
  guardC "stakeActLogic: failed when checking minted value" $
    oneOf # cs # tn # mintVal
  pconstantC ()

withdrawActLogic ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  Term s PUnit
withdrawActLogic cs tn mintVal = unTermCont $ do
  -- Check that the token was burnt once
  guardC "withdrawActLogic: failed when checking minted value" $
    oneOfWith
      # (peq # cs)
      # (peq # tn)
      # (plam $ \n -> n #== -1)
      # mintVal
  pconstantC ()
