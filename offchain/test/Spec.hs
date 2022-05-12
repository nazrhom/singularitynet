module Main (main, tests) where

import Cardano.Prelude (Text)
import Plutus.Contract (Contract)
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Test.Plutip.Contract (initAda, assertExecution, withContract)
import Test.Plutip.Predicate(shouldSucceed)
import Test.Plutip.LocalCluster (withCluster)
import Test.Tasty (TestTree, defaultMain)

main :: IO ()
main = Test.Tasty.defaultMain tests

tests :: Test.Tasty.TestTree
tests =
  Test.Plutip.LocalCluster.withCluster
    "Integration tests"
    [ assertExecution
        "Dummy contract"
        (initAda [100])
        (withContract $ const dummy)
        [ shouldSucceed ]
    ]

dummy :: Contract String EmptySchema Text ()
dummy = return ()
