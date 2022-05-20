module Test.Utils (
  succeeds,
  fails,
  returnsTrue,
  returnsFalse,
  shouldBe,
  shouldNotBe,
) where

import Test.Tasty ()
import Test.Tasty.HUnit (Assertion, assertFailure)

import Plutarch (ClosedTerm, compile, printScript)
import Plutarch.Evaluate (evaluateScript)
import Plutus.V1.Ledger.Scripts qualified as Scripts

-- Most of these are taken from Plutarch's example tests
succeeds :: forall (a :: PType). ClosedTerm a -> Assertion
succeeds x = case evaluateScript $ compile x of
  Left e -> assertFailure $ "Script evaluation failed: " <> show e
  Right _ -> pure ()

fails :: forall (a :: PType). ClosedTerm a -> Assertion
fails x = case evaluateScript $ compile x of
  Left (Scripts.EvaluationError _ _) -> mempty
  Left (Scripts.EvaluationException _ _) -> mempty
  Left e -> assertFailure $ "Script is malformed" <> show e
  Right (_, _, s) -> assertFailure $ "Script did not err: " <> printScript s

returnsTrue :: ClosedTerm PBool -> Assertion
returnsTrue x =
  succeeds $
    pif x (pconstant ()) $ ptraceError "returnsTrue: script returned False"

returnsFalse :: ClosedTerm PBool -> Assertion
returnsFalse x =
  succeeds $
    pif x (ptraceError "returnsFalse: script returned True") $ pconstant ()

shouldBe ::
  forall (a :: PType).
  PEq a =>
  ClosedTerm a ->
  ClosedTerm a ->
  Assertion
shouldBe a b =
  succeeds $
    pif
      (a #== b)
      (pconstant ())
      $ ptraceError "shouldBe: terms are not equal"

shouldNotBe ::
  forall (a :: PType).
  PEq a =>
  ClosedTerm a ->
  ClosedTerm a ->
  Assertion
shouldNotBe a b =
  succeeds $
    pif
      (pnot #$ a #== b)
      (pconstant ())
      $ ptraceError "shouldNotBe: terms are equal"
