module Utils where

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch
import Plutarch.Evaluate(evaluateBudgetedScript, evaluateScript)
import Plutarch.Prelude
import Plutus.V1.Ledger.Scripts qualified as Scripts

-- Most of these are taken from Plutarch's example tests
succeeds :: ClosedTerm PUnit -> Assertion
succeeds x = case evaluateScript $ compile x of
  Left e -> assertFailure $ "Script evaluation failed: " <> show e
  Right _ -> pure ()
  
fails :: ClosedTerm PUnit -> Assertion
fails x = case evaluateScript $ compile x of
  Left (Scripts.EvaluationError _ _) -> mempty
  Left (Scripts.EvaluationException _ _) -> mempty
  Left e -> assertFailure $ "Script is malformed" <> show e
  Right (_, _, s) -> assertFailure $ "Script did not err: " <> printScript s