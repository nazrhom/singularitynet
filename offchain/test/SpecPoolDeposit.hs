module SpecPoolDeposit (
  specPoolDeposit,
) where

import Data.Functor (void)
import Data.Text (Text)

import Ledger.Address (PaymentPubKeyHash)
import Plutus.Contract (
  Contract,
  EmptySchema,
 )
import Test.Plutip.Contract (assertExecution, withContract)
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldSucceed)
import Test.Tasty (TestTree)

import SingularityNet.Natural (Natural)
import Types (BondedPoolScripts)
import Utils (
  createBondedPool,
  poolDeposit,
  testAdminDeposit,
  testAdminWallet,
  testInitialBondedParams,
  testUserStake,
  testUserWallet,
  userHeadStake,
 )

specPoolDeposit :: BondedPoolScripts -> TestTree
specPoolDeposit scripts =
  Test.Plutip.LocalCluster.withCluster
    "Bonded Pool Validator - Pool depositing"
    [ assertExecution
        "should validate deposit"
        (testAdminWallet <> testUserWallet)
        ( withContract $
            poolDepositContract scripts testUserStake testAdminDeposit
        )
        [shouldSucceed]
    ]

poolDepositContract ::
  BondedPoolScripts ->
  Natural ->
  Natural ->
  [PaymentPubKeyHash] ->
  Contract String EmptySchema Text ()
poolDepositContract bps stakeAmt depositAmt pkhs = do
  (bpts, bpp, bondedPool) <- createBondedPool bps testInitialBondedParams
  (bondedPool', entry) <- userHeadStake pkhs bpts bpp bondedPool stakeAmt
  void $ poolDeposit bpts bpp bondedPool' [entry] depositAmt
