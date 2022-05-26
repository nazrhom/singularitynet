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
  noneOf,
  oneOf,
  oneWith,
  pconst,
  peq,
  pfalse,
  pfind,
  pflip,
  pifC,
  pletC,
  ppartition,
  ptrue,
  ptryFromUndata,
  punit,
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
            guardMint listNftCs entryTn mintVal
            stakeHeadCheck stateNftCs listNftCs stateNftTn stateOutRef inputs
    PMintInBetween entries -> unTermCont $ do
      guardMint listNftCs entryTn mintVal
      entriesF <- tcont $ pletFields @["previousEntry", "currentEntry"] entries
      let prevEntry = entriesF.previousEntry
          currEntry = entriesF.currentEntry
      stakeInBetweenCheck
        stateNftCs
        listNftCs
        stateNftTn
        prevEntry
        currEntry
        inputs
    PMintEnd lastEntry' -> unTermCont $ do
      let lastEntry :: Term s PTxOutRef
          lastEntry = pfromData $ pfield @"_0" # lastEntry'
      guardMint listNftCs entryTn mintVal
      stakeEndCheck stateNftCs listNftCs stateNftTn lastEntry inputs

-- TODO: Inductive conditions related to withdrawing not implemented
listRemoveCheck :: forall (s :: S) .
  Term s PValue ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PBurningAction ->
  Term s PUnit
listRemoveCheck mintVal (listNftCs, entryTn) = flip pmatch $ \case
  PBurnHead _poolState -> unTermCont $ do
    guardBurn listNftCs entryTn mintVal
    -- TODO
  PBurnOther _prevEntry -> unTermCont $ do
    guardBurn listNftCs entryTn mintVal
    -- TODO

plistNFTPolicyUntyped ::
  forall (s :: S). Term s (PData :--> PData :--> PData :--> PData :--> PUnit)
plistNFTPolicyUntyped = plam $ \stateNftCs stateNftTn mintAct ctx ->
  plistNFTPolicy # unTermCont (ptryFromUndata stateNftCs)
    # unTermCont (ptryFromUndata stateNftTn)
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

-- Build the token name from the signatory's `PPubKeyHash`
mkEntryTn :: Term s PPubKeyHash -> Term s PTokenName
mkEntryTn pkh = pcon . PTokenName $ pblake2b_256 # pto pkh

-- Functions for validating the minted value
guardMint ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  TermCont s (Term s PUnit)
guardMint listNftCs entryTn mint =
  guardC "plistNFTPolicy: failed when checking minted value" $
    oneOf # listNftCs # entryTn # mint

guardBurn ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  TermCont s (Term s PUnit)
guardBurn listNftCs entryTn mint =
  guardC "plistNFTPolicy: failed when checking minted value" $
    checkBurn listNftCs entryTn mint

-- Functions for validating the inputs
stakeHeadCheck ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
stakeHeadCheck stateNftCs listNftCs stateNftTn stateOutRef inputs = do
  -- Find state UTXO in inputs and fail if it does not have the state NFT
  _ <- flip pfind inputs . plam $ \input -> unTermCont $ do
    inputF <- tcont $ pletFields @["outRef", "resolved"] input
    let outRef = inputF.outRef
        val = pfield @"value" # inputF.resolved
    pifC
      (pdata outRef #== pdata stateOutRef)
      ( pif
          (oneOf # stateNftCs # stateNftTn # val)
          ptrue
          $ ptraceError
            "stakeHeadCheck: txOutRef does not have pool NFT"
      )
      pfalse
  -- Check that all inputs are either
  --  1. The state UTXO
  --  2. A UTXO with no entry NFT nor state NFT
  guardC "stakeHeadCheck: failed when checking inputs for PStakeHead" $
    pflip pall inputs . plam $ \input -> unTermCont $ do
      inputF <- tcont $ pletFields @["outRef", "resolved"] input
      let outRef = inputF.outRef
          val = pfield @"value" # inputF.resolved
      pure $
        pdata outRef #== pdata stateOutRef
          #|| ( pnot # hasListNft listNftCs val
                  #&& pnot # hasStateNft stateNftCs stateNftTn val
              )

  pure punit
stakeInBetweenCheck ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PTxOutRef ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
stakeInBetweenCheck stateNftCs listNftCs stateNftTn prevEntry currEntry inputs =
  do
    -- Separate list entries from other inputs, fail if they don't have the
    -- list NFT
    splitInputs <- pletC . flip ppartition inputs . plam $
      \txInfo -> unTermCont $ do
        txInfoF <- tcont $ pletFields @'["outRef", "resolved"] txInfo
        let outRef = pdata txInfoF.outRef
            val = pfromData $ pfield @"value" # txInfoF.resolved
        pifC
          (outRef #== pdata prevEntry #|| outRef #== pdata currEntry)
          ( pif
              (hasListNft listNftCs val)
              ptrue
              ( ptraceError
                  "stakeInBetweenCheck: txOutRef does not have list NFT"
              )
          )
          pfalse

    splitInputsF <- tcont $ pletFields @'["_0", "_1"] splitInputs
    let entries = pfromData splitInputsF._0
        otherInputs = pfromData splitInputsF._1

    guardC "stakeInBetweenCheck: expected exactly two list entries" $
      plength # entries #== 2

    guardC "stakeInBetweenCheck: state utxo not allowed" $
      pflip pall otherInputs . plam $ \txInfo ->
        let val :: Term s PValue
            val = pfield @"value" #$ pfield @"resolved" # txInfo
         in noneOf # stateNftCs # stateNftTn # val

    pure punit

stakeEndCheck ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
stakeEndCheck stateNftCs listNftCs stateNftTn lastEntry inputs = do
  -- Find entry UTXO in inputs and fail if it does not have the list NFT
  _ <- flip pfind inputs . plam $ \input -> unTermCont $ do
    inputF <- tcont $ pletFields @["outRef", "resolved"] input
    let outRef = inputF.outRef
        val = pfield @"value" # inputF.resolved
    pifC
      (pdata outRef #== pdata lastEntry)
      ( pif
          (hasListNft listNftCs val)
          ptrue
          $ ptraceError "stakeHeadCheck: txOutRef does not have list NFT"
      )
      pfalse
  -- Check that all inputs are either
  --  1. The entry UTXO
  --  2. A UTXO with no state NFT nor entry NFT
  guardC "stakeHeadCheck: failed when checking inputs for PStakeHead" $
    pflip pall inputs . plam $ \input -> unTermCont $ do
      inputF <- tcont $ pletFields @["outRef", "resolved"] input
      let outRef = inputF.outRef
          val = pfield @"value" # inputF.resolved
      pure $
        pdata outRef #== pdata lastEntry
          #|| ( pnot # hasListNft listNftCs val
                  #&& pnot # hasStateNft stateNftCs stateNftTn val
              )
  pure punit

-- Check that the token was burnt once
checkBurn ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  Term s PBool
checkBurn cs tn mintVal = checkBurn' # cs # tn # mintVal
  where
    checkBurn' ::
      Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PBool)
    checkBurn' = phoistAcyclic $
      plam $ \cs tn mintVal ->
        oneWith
          # (peq # cs)
          # (peq # tn)
          # (plam $ \n -> n #== -1)
          # mintVal

-- Has list NFT predicate
hasListNft ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PValue ->
  Term s PBool
hasListNft listNftCs val = hasListNft' # listNftCs # val
  where
    hasListNft' :: Term s (PCurrencySymbol :--> PValue :--> PBool)
    hasListNft' = phoistAcyclic $
      plam $ \cs val ->
        oneWith
          # (peq # cs)
          # (pconst ptrue)
          # (peq # 1)
          # val

-- Has state NFT predicate
hasStateNft ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  Term s PBool
hasStateNft stateNftCs stateNftTn val = hasStateNft' # stateNftCs # stateNftTn # val
  where
    hasStateNft' :: Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PBool)
    hasStateNft' = phoistAcyclic $
      plam $ \cs tn val ->
        oneOf # cs # tn # val
