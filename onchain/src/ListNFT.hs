module ListNFT (
  plistNFTPolicy,
  plistNFTPolicyUntyped,
) where

import Plutarch.Api.V1 (
  PCurrencySymbol,
  PPubKeyHash,
  PScriptContext,
  PTokenName (PTokenName),
  PTxInInfo,
  PTxOutRef,
  PValue,
 )
import Plutarch.Api.V1.Scripts ()
import Plutarch.Unsafe (punsafeCoerce)

import PTypes (
  PListAction (PListInsert, PListRemove),
  PMintingAction (PMintHead, PMintInBetween, PMintEnd),
  PBurningAction (PBurnHead, PBurnOther),
 )

import Plutarch.Crypto (pblake2b_256)
import Utils (
  getCs,
  guardC,
  oneOf,
  oneWith,
  peq,
  pflip,
  pletC,
  ptryFromUndata,
  porList
 )
import InductiveLogic (
  consumesStateUtxoGuard,
  consumesEntriesGuard,
  consumesEntryGuard,
  hasNoNft,
  inputPredicate
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

plistNFTPolicy ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol
        :--> PTokenName
        :--> PListAction
        :--> PScriptContext
        :--> PUnit
    )
plistNFTPolicy = plam $ \stateNftCs stateNftTn listAct ctx' -> unTermCont $ do
  -- This CurrencySymbol is only used for parametrization
  _cs <- pletC stateNftCs
  ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  -- Own CurrencySymbol
  listNftCs <- getCs ctx.purpose
  txInfo <- tcont $ pletFields @'["signatories", "mint", "inputs"] ctx.txInfo
  -- Get a *single* signatory or fail
  signatory <- getSignatory txInfo.signatories
  -- Make token name from signatory's public key hash
  let entryTn = mkEntryTn signatory
      -- Save state and list tokens for later
      stateTok = (stateNftCs, stateNftTn)
      listTok = (listNftCs, entryTn)
  -- Dispatch to appropiate handler based on redeemer
  pure $
    pmatch listAct $ \case
      PListInsert mintAct' ->
        let mintAct = pfield @"_0" # mintAct'
        in listInsertCheck txInfo.inputs txInfo.mint stateTok listTok mintAct
      PListRemove burnAct' ->
        let burnAct = pfield @"_0" # burnAct'
        in listRemoveCheck txInfo.mint listTok burnAct

plistNFTPolicyUntyped ::
  forall (s :: S). Term s (PData :--> PData :--> PData :--> PData :--> PUnit)
plistNFTPolicyUntyped = plam $ \stateNftCs stateNftTn mintAct ctx ->
  plistNFTPolicy # unTermCont (ptryFromUndata stateNftCs)
    # unTermCont (ptryFromUndata stateNftTn)
    # unTermCont (ptryFromUndata mintAct)
    # punsafeCoerce ctx

listInsertCheck :: forall (s :: S) .
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PValue ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PMintingAction ->
  Term s PUnit
listInsertCheck inputs mintVal (stateNftCs, stateNftTn) (listNftCs, entryTn) =
  flip pmatch $ \case
    PMintHead stateOutRef' -> unTermCont $ do
      let stateOutRef :: Term s PTxOutRef
          stateOutRef = pfromData $ pfield @"_0" # stateOutRef'
      mintGuard listNftCs entryTn mintVal
      mintHeadGuard stateNftCs listNftCs stateNftTn stateOutRef inputs
    PMintInBetween entries -> unTermCont $ do
      mintGuard listNftCs entryTn mintVal
      entriesF <- tcont $ pletFields @["previousEntry", "currentEntry"] entries
      let prevEntry = entriesF.previousEntry
          currEntry = entriesF.currentEntry
      mintInBetweenGuard stateNftCs listNftCs prevEntry currEntry inputs
    PMintEnd lastEntry' -> unTermCont $ do
      let lastEntry :: Term s PTxOutRef
          lastEntry = pfromData $ pfield @"_0" # lastEntry'
      mintGuard listNftCs entryTn mintVal
      mintEndGuard stateNftCs listNftCs lastEntry inputs

-- TODO: Inductive conditions related to withdrawing not implemented
listRemoveCheck :: forall (s :: S) .
  Term s PValue ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PBurningAction ->
  Term s PUnit
listRemoveCheck mintVal (listNftCs, entryTn) = flip pmatch $ \case
  PBurnHead _poolState -> unTermCont $ do
    burnGuard listNftCs entryTn mintVal
    -- TODO
  PBurnOther _prevEntry -> unTermCont $ do
    burnGuard listNftCs entryTn mintVal
    -- TODO


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

-- Build the token name from the signatory's `PPubKeyHash`
mkEntryTn :: Term s PPubKeyHash -> Term s PTokenName
mkEntryTn pkh = pcon . PTokenName $ pblake2b_256 # pto pkh

mintGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  TermCont s (Term s PUnit)
mintGuard listNftCs entryTn mint =
  guardC "mintGuard: failed when checking minted value" $
    oneOf # listNftCs # entryTn # mint

burnGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  TermCont s (Term s PUnit)
burnGuard listNftCs entryTn mint =
  guardC "mintGuard: failed when checking minted value" $
    burnCheck listNftCs entryTn mint

-- Functions for validating the inputs
mintHeadGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
mintHeadGuard stateNftCs listNftCs stateNftTn stateOutRef inputs = do
  -- We check that the state UTXO is consumed
  consumesStateUtxoGuard stateOutRef inputs stateNftCs stateNftTn
  -- We check that the other inputs are not state nor list UTXOs
  noNftGuard stateNftCs listNftCs [stateOutRef] inputs

mintInBetweenGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PTxOutRef ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
mintInBetweenGuard stateNftCs listNftCs prevEntry currEntry inputs = do
  -- We check that both list entries are consumed
  consumesEntriesGuard prevEntry currEntry inputs listNftCs
  -- We check that the other inputs are not state nor list UTXOs
  noNftGuard stateNftCs listNftCs [prevEntry, currEntry] inputs

mintEndGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
mintEndGuard stateNftCs listNftCs lastEntry inputs = do
  -- Find entry UTXO in inputs and fail if it does not have the list NFT
  consumesEntryGuard lastEntry inputs listNftCs
  -- We check that the other inputs are not state nor list UTXOs
  noNftGuard stateNftCs listNftCs [lastEntry] inputs
  
-- Check that all unprotected inputs contain no state nor list NFTs
noNftGuard :: forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  [Term s PTxOutRef] ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
noNftGuard stateNftCs listNftCs protectedInputs inputs =
  guardC "noNftGuard: unallowed input in the transaction contains state or list\
         \ NFT" $
    pflip pall inputs . inputPredicate $ \outRef val ->
      let equalToSome :: [Term s PTxOutRef] -> Term s PBool
          equalToSome = porList . fmap (\o -> pdata outRef #== pdata o)
      in equalToSome protectedInputs #|| hasNoNft stateNftCs listNftCs val

-- Check that the token was burnt once
burnCheck ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  Term s PBool
burnCheck cs tn mintVal = burnCheck' # cs # tn # mintVal
  where
    burnCheck' ::
      Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PBool)
    burnCheck' = phoistAcyclic $
      plam $ \cs tn mintVal ->
        oneWith
          # (peq # cs)
          # (peq # tn)
          # (plam $ \n -> n #== -1)
          # mintVal
