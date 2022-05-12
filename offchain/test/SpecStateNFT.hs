module SpecStateNFT(specStateNFT) where

import Data.Text(Text)

import Plutus.V1.Ledger.Api
import Ledger.Ada
import Ledger.Constraints
import Ledger.Address
import Ledger.Scripts
import Plutus.Contract

import Test.Plutip.Contract
import Test.Plutip.LocalCluster
import Test.Plutip.Predicate
import Test.Tasty (TestTree, localOption, testGroup)

import Common.Settings(bondedStakingTokenName)


specStateNFT :: Script -> TestTree
specStateNFT _policyScript =
  Test.Plutip.LocalCluster.withCluster
    "State NFT Policy"
    [
    assertExecution
        "should validate correct transaction"
        (initAda [100])
        (withContract correctMint)
        [ shouldSucceed ]
    , assertExecution
        "should validate correct transaction with spurious tokens"
        (initAda [100])
        (withContract spuriousButCorrectMint)
        [ shouldSucceed ]
    , assertExecution
        "should not mint more than once"
        (initAda [100])
        (withContract multipleMint)
        [ shouldFail ]
    , assertExecution
        "should not consume the wrong outRef"
        (initAda [100])
        (withContract wrongOutRefMint)
        [ shouldFail ]
    ]

correctMint :: [PaymentPubKeyHash] -> Contract String EmptySchema Text ()
correctMint [] = do
    pure ()
correctMint _ = error "correctMint: wrong number of wallets"

spuriousButCorrectMint ::
    [PaymentPubKeyHash] ->
    Contract String EmptySchema Text ()
spuriousButCorrectMint [] = pure ()
spuriousButCorrectMint _ =
    error "spuriousButCorrectMint: wrong number of wallets"

multipleMint :: [PaymentPubKeyHash] -> Contract String EmptySchema Text ()
multipleMint [] = pure ()
multipleMint _ = error "multipleMint: wrong number of wallets"

wrongOutRefMint :: [PaymentPubKeyHash] -> Contract String EmptySchema Text ()
wrongOutRefMint [] = pure ()
wrongOutRefMint _ = error "wrongOutRefMint: wrong number of wallets"

createPolicy :: TxOutRef -> Script -> MintingPolicy
createPolicy outRef script = undefined