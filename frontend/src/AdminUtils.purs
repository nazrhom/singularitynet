module AdminUtils
  ( calculateRewards
  , mkEntryUpdateList
  )
  where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM, liftedM, throwContractError)
import Contract.Numeric.Rational ((%), Rational)
import Contract.PlutusData (PlutusData, Datum(Datum), fromData, getDatumByHash, toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (ValidatorHash)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Contract.TxConstraints (TxConstraints, mustPayToScript, mustSpendScriptOutput)
import Contract.Value (mkTokenName, singleton)
import Data.BigInt (BigInt)
import Types (BondedStakingAction(AdminAct), BondedStakingDatum(AssetDatum, EntryDatum), BondedPoolParams(BondedPoolParams), Entry(Entry))
import Types.Redeemer (Redeemer(Redeemer))
import Utils (logInfo_, mkRatUnsafe, roundUp)

-- | Creates a constraint and lookups list for updating each user entry
mkEntryUpdateList
  :: BondedPoolParams
  -> ValidatorHash
  -> (ByteArray /\ TransactionInput /\ TransactionOutput)
  -> Contract ()
       ( Tuple (TxConstraints Unit Unit)
           (ScriptLookups.ScriptLookups PlutusData)
       )
mkEntryUpdateList
  (BondedPoolParams { interest, bondedAssetClass, assocListCs })
  valHash
  (_ /\ txIn /\ txOut) = do
  -- Get the Entry datum of the old assoc. list element
  dHash <- liftContractM
    "mkEntryUpdateList: Could not get Entry Datum Hash"
    (unwrap txOut).dataHash
  logInfo_ "mkEntryUpdateList: datum hash" dHash
  listDatum <-
    liftedM
      "mkEntryUpdateList: Cannot get Entry's datum" $ getDatumByHash dHash
  bondedListDatum :: BondedStakingDatum <-
    liftContractM
      "mkEntryUpdateList: Cannot extract NFT State datum"
      $ fromData (unwrap listDatum)
  -- The get the entry datum
  case bondedListDatum of
    EntryDatum { entry } -> do
      let e = unwrap entry
      calculatedRewards <-
        calculateRewards
          interest
          e.deposited
      assocListTn <-
        liftContractM
          "mkEntryUpdateList: Could not create token name for user"
          $ mkTokenName e.key
      -- Update the entry datum
      let
        recentRewards = roundUp calculatedRewards
        updatedDeposited = e.deposited + recentRewards
        newRewards = e.rewards + mkRatUnsafe (recentRewards % one)
        -- Datum and redeemer creation
        entryDatum = Datum $ toData $ EntryDatum
          { entry: Entry $ e
              { newDeposit = zero -- reset to zero
              , deposited = updatedDeposited
              , staked = updatedDeposited -- redundant
              , rewards = newRewards
              }
          }
        valRedeemer = Redeemer $ toData AdminAct
        -- Build asset datum and value types
        assetDatum = Datum $ toData AssetDatum
        assetParams = unwrap bondedAssetClass
        assetCs = assetParams.currencySymbol
        assetTn = assetParams.tokenName
        depositValue = singleton assetCs assetTn recentRewards
        entryValue = singleton assocListCs assocListTn one

        -- Build constraints and lookups
        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
            [ mustPayToScript valHash assetDatum depositValue
            , mustPayToScript valHash entryDatum entryValue
            , mustSpendScriptOutput txIn valRedeemer
            ]
      entryDatumLookup <-
        liftContractM
          "mkEntryUpdateList: Could not create state datum lookup"
          $ ScriptLookups.datum entryDatum
      pure (constraints /\ entryDatumLookup)
    _ -> throwContractError
      "mkEntryUpdateList: Datum not Entry constructor"

-- | Calculates user rewards
calculateRewards
  :: Rational -- interest
  -> BigInt -- deposited
  -> Contract () Rational
calculateRewards interest deposited = do
  when (deposited == zero) $
    throwContractError "calculateRewards: totalDeposited is zero"
  let
    recentRewards = interest * mkRatUnsafe (deposited % one)
  when (recentRewards < zero) $ throwContractError
    "calculateRewards: invalid rewards amount"
  pure recentRewards