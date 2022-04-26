module ListNFT(
    pbondedListNFTPolicy
    , hbondedListNFTPolicy
) where

import Plutarch.Api.V1
    ( mkMintingPolicy,
      PScriptContext,
      PPubKeyHash,
      PCurrencySymbol,
      PTokenName(PTokenName),
      PValue )
import Plutarch.Api.V1.Scripts ()
import Plutarch.Unsafe ( punsafeCoerce )

import Plutus.V1.Ledger.Api ( MintingPolicy, CurrencySymbol )

import Types ( PMintingAction )
import Utils ( peq, oneOfWith, pletC, getCs, pconstantC, guardC )
import Plutarch.Crypto (pblake2b_256)

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
-- (that's why the redeemer isn't used yet). `burnsOrMintsOnce` ought to be
-- split in two parts, one for each of the two use cases (staking and
-- withdrawing).
pbondedListNFTPolicy ::
    forall (s :: S) .
    Term s PCurrencySymbol ->
    Term s (
       PMintingAction :-->
       PScriptContext :-->
       PUnit
    )
pbondedListNFTPolicy cs' = plam $ \_mintAct ctx' -> unTermCont $ do
    -- This CurrencySymbol is only used for parametrization
    _cs <- pletC cs'
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    -- Own CurrencySymbol
    cs <- getCs ctx.purpose
    txInfo <- tcont $ pletFields @'["signatories", "mint"] ctx.txInfo
    -- Get a *single* signatory or fail
    signatory <- getSignatory txInfo.signatories
    -- Calculate TokenName based on PubKeyHash
    let tn :: Term s PTokenName
        tn = pcon . PTokenName $ pblake2b_256 # pto signatory
    -- Check the token was minted or burnt *once*
    guardC "failed when checking minted value" $
        burnsOrMintsOnce cs tn txInfo.mint

    pconstantC ()

hbondedListNFTPolicy :: CurrencySymbol -> MintingPolicy
hbondedListNFTPolicy cs =
    mkMintingPolicy $ punsafeCoerce $ pbondedListNFTPolicy $ pconstant cs
    
getSignatory ::
    forall (s :: S) .
    Term s (PBuiltinList (PAsData PPubKeyHash)) ->
    TermCont s (Term s PPubKeyHash)
getSignatory ls = pure . pmatch ls $ \case
    PCons pkh' ps -> pif (pnull # ps)
                         (pfromData pkh')
                         (ptraceError "transaction has more than one signatory")
    PNil -> ptraceError "empty list of signatories"
    

burnsOrMintsOnce ::
    forall (s :: S) .
    Term s PCurrencySymbol ->
    Term s PTokenName ->
    Term s PValue ->
    Term s PBool
burnsOrMintsOnce cs tn val =
    oneOfWith
        # (peq # cs)
        # (peq # tn)
        # (plam $ \n -> n #== 1 #|| n #== (-1))
        # val