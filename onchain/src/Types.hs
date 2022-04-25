{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Types (
  BondedPoolParams (BondedPoolParams, operator, bondedStakingStateCs),
  PBondedPoolParams,
  BondedStakingAction (..),
  PBondedStakingAction (..),
  BondedStakingDatum (..),
  PBondedStakingDatum (..),
  Entry (
    Entry,
    key,
    value,
    next,
    sizeLeft,
    newDeposit,
    deposited,
    staked,
    rewards
  ),
  PEntry,
  AssetClass (AssetClass, unAssetClass),
  acCurrencySymbol,
  acTokenName,
  mkAssetClass,
  PAssetClass,
) where

import GHC.Generics qualified as GHC
import Generics.SOP (Generic, I (I))

import Plutarch.Api.V1 (PMaybeData, PPOSIXTime, PTokenName)
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
 )
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  PubKeyHash,
  TokenName,
 )
import PlutusTx (unstableMakeIsData)
import PlutusTx.Builtins.Internal (BuiltinByteString)

import Numeric (
  NatRatio,
  Natural,
  PNatRatio,
  PNatural,
 )

-- | An `AssetClass` is simply a wrapper over a pair (CurrencySymbol, TokenName)
newtype PAssetClass (s :: S)
  = PAssetClass
      ( Term s (PBuiltinPair PCurrencySymbol PTokenName)
      )
  deriving stock (GHC.Generic)
  deriving
    (PlutusType)
    via ( DerivePNewtype
            PAssetClass
            (PBuiltinPair PCurrencySymbol PTokenName)
        )

newtype AssetClass = AssetClass
  { unAssetClass :: (CurrencySymbol, TokenName)
  }
  deriving stock (GHC.Generic, Show)
  deriving anyclass (Generic)

unstableMakeIsData ''AssetClass

deriving via
  ( DerivePConstantViaNewtype
      AssetClass
      PAssetClass
      (PBuiltinPair PCurrencySymbol PTokenName)
  )
  instance
    (PConstant AssetClass)

instance PUnsafeLiftDecl PAssetClass where
  type PLifted PAssetClass = AssetClass

-- | A Natural is a wrapper over integer
mkAssetClass :: CurrencySymbol -> TokenName -> AssetClass
mkAssetClass cs tn = AssetClass (cs, tn)

acCurrencySymbol :: AssetClass -> CurrencySymbol
acCurrencySymbol = fst . unAssetClass
acTokenName :: AssetClass -> TokenName
acTokenName = snd . unAssetClass

{- | Bonded pool's parameters

     These parametrise the staking pool contract. However, the one parameter
     that makes each contract truly unique is `nftCs` (the NFT's
     CurrencySymbol).

     The currency symbol of the associated list (`assocListCs`) is also uniquely
     associated to the `nftCs`.
-}
newtype PBondedPoolParams (s :: S)
  = PBondedPoolParams
      ( Term
          s
          ( PDataRecord
              '[ "iterations" ':= PNatural
               , "start" ':= PPOSIXTime
               , "end" ':= PPOSIXTime
               , "userLength" ':= PPOSIXTime
               , "bondingLength" ':= PPOSIXTime
               , "interest" ':= PNatRatio
               , "minStake" ':= PNatural
               , "maxStake" ':= PNatural
               , "admin" ':= PPubKeyHash
               , "bondedAssetClass" ':= PAssetClass
               , "nftCs" ':= PCurrencySymbol
               , "assocListCs" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PBondedPoolParams

data BondedPoolParams = BondedPoolParams
  { operator :: PubKeyHash
  , bondedStakingStateCs :: CurrencySymbol
  }
  deriving stock (GHC.Generic, Show)

unstableMakeIsData ''BondedPoolParams

deriving via
  (DerivePConstantViaData BondedPoolParams PBondedPoolParams)
  instance
    (PConstant BondedPoolParams)

instance PUnsafeLiftDecl PBondedPoolParams where
  type PLifted PBondedPoolParams = BondedPoolParams

{- | Associacion list's entry

     An entry in the association list. It keeps track of how much a user staked
     and the pending rewards. It also has a reference to the next entry in the
     list, which might be empty if it is the final element.
-}
data PEntry (s :: S)
  = PEntry
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PByteString
               , "sizeLeft" ':= PNatural
               , "newDeposit" ':= PMaybeData PNatural
               , "deposited" ':= PNatural
               , "staked" ':= PNatural
               , "rewards" ':= PNatRatio
               , "value" ':= PBuiltinPair PNatural PNatRatio
               , "next" ':= PMaybeData PByteString
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PEntry

data Entry = Entry
  { key :: BuiltinByteString
  , sizeLeft :: Natural
  , newDeposit :: Maybe Natural
  , deposited :: Natural
  , staked :: Natural
  , rewards :: NatRatio
  , value :: (Natural, NatRatio)
  , next :: Maybe BuiltinByteString
  }
  deriving stock (Show)

unstableMakeIsData ''Entry

deriving via
  (DerivePConstantViaData Entry PEntry)
  instance
    (PConstant Entry)

instance PUnsafeLiftDecl PEntry where
  type PLifted PEntry = Entry

{- | Bonded pool's state

     It can either contain:

     1. A reference to the on-chain association list of stakees-stakes (in the
     case of the pool UTXO)

     2. An entry in the association list (created by the stakers when using
     the StakeAct redeemer)

     3. A dummy datum (in the case of the stake UTXOs)
-}
data PBondedStakingDatum (s :: S)
  = PStateDatum
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PBuiltinPair (PMaybeData PByteString) PNatural
               ]
          )
      )
  | PEntryDatum
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PEntry
               ]
          )
      )
  | PAssetDatum (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PBondedStakingDatum

data BondedStakingDatum
  = StateDatum (Maybe BuiltinByteString) Natural
  | EntryDatum Entry
  | AssetDatum
  deriving stock (Show, GHC.Generic)

unstableMakeIsData ''BondedStakingDatum

deriving via
  (DerivePConstantViaData BondedStakingDatum PBondedStakingDatum)
  instance
    (PConstant BondedStakingDatum)

instance PUnsafeLiftDecl PBondedStakingDatum where
  type PLifted PBondedStakingDatum = BondedStakingDatum

{- | Minting redeemers

     These are used for staking and withdrawing funds but they are *not* used
     for consuming the bonded pool's contract, but rather for minting the NFTs
     that comprise each entry in the association list.
-}
data PMintingAction (s :: S)
  = PStake (Term s (PDataRecord '[]))
  | PWithdraw (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PMintingAction

data MintingAction
  = Stake
  | Withdraw
  deriving stock (GHC.Generic)

unstableMakeIsData ''MintingAction

deriving via
  (DerivePConstantViaData MintingAction PMintingAction)
  instance
    (PConstant MintingAction)

instance PUnsafeLiftDecl PMintingAction where
  type PLifted PMintingAction = MintingAction

{- | Validator redeemers

     These are used by the admin to deposit the rewards and close the pool and
     withdraw the rewards unclaimed.

     These are used by the stakers to deposit their *initial* stake (after that
     they only update their respective entry) and withdrawing their rewards.
-}
data PBondedStakingAction (s :: S)
  = PAdminAct (Term s (PDataRecord '["_0" ':= PNatural]))
  | PStakeAct
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PNatural
               , "_1" ':= PPubKeyHash
               ]
          )
      )
  | PWithdrawAct (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PCloseAct (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances PBondedStakingAction

data BondedStakingAction
  = AdminAct Natural
  | StakeAct Natural PubKeyHash
  | WithdrawAct PubKeyHash
  | CloseAct
  deriving stock (Show)

unstableMakeIsData ''BondedStakingAction

deriving via
  (DerivePConstantViaData BondedStakingAction PBondedStakingAction)
  instance
    (PConstant BondedStakingAction)

instance PUnsafeLiftDecl PBondedStakingAction where
  type PLifted PBondedStakingAction = BondedStakingAction
