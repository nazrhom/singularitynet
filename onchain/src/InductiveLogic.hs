module InductiveLogic (
  consumesStateUtxoGuard,
  consumesEntriesGuard,
  consumesEntryGuard,
  doesNotConsumeAssetGuard,
  hasStateNft,
  hasListNft,
  hasNoNft,
  inputPredicate,
) where

{-
    This module gathers functions useful for validating the inductive conditions
    of the associative on-chain list.

-}
import Plutarch.Api.V1 (
  PCurrencySymbol,
  PTokenName,
  PTxInInfo,
  PTxOutRef,
  PValue,
 )

import Utils (
  allWith,
  guardC,
  oneOf,
  oneWith,
  pconst,
  peq,
  pfalse,
  pfind,
  pflip,
  pneq,
  ptrue,
  punit,
 )

import PTypes (PBondedStakingDatum (PAssetDatum))

-- | Fail if state UTXO is not in inputs or if it does not have the state token
consumesStateUtxoGuard ::
  forall (s :: S).
  Term s PTxOutRef ->
  -- |^ Redeemer's pool UTXO
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  -- |^ Transaction's inputs
  Term s PCurrencySymbol ->
  -- |^ Pool's currency symbol
  Term s PTokenName ->
  -- |^ Pool's token name
  TermCont s (Term s PUnit)
consumesStateUtxoGuard stateOutRef inputs stateNftCs stateNftTn = do
  _ <- flip pfind inputs . inputPredicate $ \outRef val ->
    pif
      (pdata outRef #== pdata stateOutRef)
      ( pif
          (hasStateNft stateNftCs stateNftTn val)
          ptrue
          $ ptraceError
            "consumesStateUtxoGuard: txOutRef does not have pool NFT"
      )
      pfalse
  pure punit

{- | Fails if the two entries are not present in inputs or they don't have a
 list token
-}
consumesEntriesGuard ::
  forall (s :: S).
  Term s PTxOutRef ->
  -- |^ Previous entry UTXO
  Term s PTxOutRef ->
  -- |^ Current entry UTXO
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  -- |^ Transaction's inputs
  Term s PCurrencySymbol ->
  -- |^ List's currency symbol
  TermCont s (Term s PUnit)
consumesEntriesGuard prevEntry currEntry inputs listNftCs = do
  entries <- pure . pflip pfilter inputs . inputPredicate $ \outRef val ->
    let prevEntryD = pdata prevEntry
        currEntryD = pdata currEntry
        outRefD = pdata outRef
     in pif
          (outRefD #== prevEntryD #|| outRefD #== currEntryD)
          ( pif
              (hasListNft listNftCs val)
              ptrue
              ( ptraceError
                  "consumesEntriesGuard: entry does not have list \
                  \NFT"
              )
          )
          pfalse
  guardC "consumesEntriesGuard: number of entries is not two" $
    plength # entries #== 2

-- | Fails if entry is not present in inputs or it does not have the list token
consumesEntryGuard ::
  forall (s :: S).
  Term s PTxOutRef ->
  -- |^ List entry
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  -- |^ Transaction's inputs
  Term s PCurrencySymbol ->
  -- |^ List's currency symbol
  TermCont s (Term s PUnit)
consumesEntryGuard entry inputs listNftCs = do
  _ <- pure . flip pfind inputs . inputPredicate $ \outRef val ->
    let entryD = pdata entry
        outRefD = pdata outRef
     in pif
          (outRefD #== entryD)
          ( pif
              (hasListNft listNftCs val)
              ptrue
              ( ptraceError
                  "consumesEntryGuard: entry does not have list \
                  \NFT"
              )
          )
          pfalse
  pure punit

doesNotConsumeAssetGuard ::
  forall (s :: S).
  Term s PBondedStakingDatum ->
  TermCont s (Term s PUnit)
doesNotConsumeAssetGuard datum = do
  result <- pure . pmatch datum $ \case
    PAssetDatum _ -> ptrue
    _ -> pfalse
  guardC "doesNotConsumeAssetGuard: tx consumes asset utxo" result

-- | Auxiliary function for building predicates on PTxInInfo's
inputPredicate ::
  forall (s :: S).
  (Term s PTxOutRef -> Term s PValue -> Term s PBool) ->
  Term s (PAsData PTxInInfo :--> PBool)
inputPredicate pred = plam $ \input ->
  pletFields @["outRef", "resolved"] (pfromData input) $ \inputF ->
    let outRef = pfromData inputF.outRef
        val = pfield @"value" # inputF.resolved
     in pred outRef val

-- | Returns `ptrue` if value contains one token with the list's currency symbol
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

-- | Returns `ptrue` if value contains the pool's state token
hasStateNft ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  Term s PBool
hasStateNft stateNftCs stateNftTn val =
  hasStateNft' # stateNftCs # stateNftTn # val
  where
    hasStateNft' :: Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PBool)
    hasStateNft' = phoistAcyclic $
      plam $ \cs tn val ->
        oneOf # cs # tn # val

{- | Returns `ptrue` if value contains neither the pool's state token nor entry
 token
-}
hasNoNft ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PValue ->
  Term s PBool
hasNoNft stateNftCs listNftCs val = hasNoNft' # stateNftCs # listNftCs # val
  where
    hasNoNft' ::
      forall (s :: S).
      Term
        s
        ( PCurrencySymbol
            :--> PCurrencySymbol
            :--> PValue
            :--> PBool
        )
    hasNoNft' = phoistAcyclic $
      plam $ \stateCs listCs val ->
        allWith
          # (plam $ \cs -> pneq # cs # stateCs #&& pneq # cs # listCs)
          # pconst ptrue
          # pconst ptrue
          # val
