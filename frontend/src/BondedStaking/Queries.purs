module Queries (getAdminUtxos) where

import Contract.Prelude

import Contract.Address
  ( Address
  , PaymentPubKeyHash(..)
  , ownPaymentPubKeyHash
  , pubKeyHashAddress
  )
import Contract.Monad (Contract(..), liftedM)
import Contract.Transaction
  ( TransactionInput(..)
  , TransactionOutputWithRefScript(..)
  )
import Contract.Utxos (UtxoMap, utxosAt)
import Data.Map (values, toUnfoldable)

-- | Retrieve the UTxOs of the administrator. Meant to be used as part
-- of defining the initial bonded pool's arguments.
getAdminUtxos
  :: Contract () (Array (TransactionInput /\ TransactionOutputWithRefScript))
getAdminUtxos = toUnfoldable <$> do
  addr :: Address <-
    flip pubKeyHashAddress Nothing <$>
      liftedM "getAdminUtxos: could not get own PubKeyHash" ownPaymentPubKeyHash
  liftedM "getAdminUtxos: could not get utxos from address" (utxosAt addr)

