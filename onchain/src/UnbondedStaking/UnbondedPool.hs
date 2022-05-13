{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module UnbondedStaking.UnbondedPool (
  punbondedPoolValidator,
  punbondedPoolValidatorUntyped,
) where

import Plutarch.Api.V1 (
  PScriptContext,
 )
import Plutarch.Api.V1.Maybe ()
import Plutarch.Unsafe (punsafeCoerce)

import UnbondedStaking.Types (
  PUnbondedPoolParams,
  PUnbondedStakingAction (
  ),
  PUnbondedStakingDatum,
 )
import Utils (
  ptryFromUndata,
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
    plam $ \_ _ _ _ -> unTermCont $ do
      pure $ pconstant ()


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
punbondedPoolValidatorUntyped = plam $ \pparams' dat' act' ctx' -> unTermCont $ do
  pure $
    punbondedPoolValidator
      # unTermCont (ptryFromUndata pparams')
      # unTermCont (ptryFromUndata dat')
      # unTermCont (ptryFromUndata act')
      # punsafeCoerce ctx'
