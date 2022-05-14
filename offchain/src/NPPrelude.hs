module NPPrelude (
  module Prelude,
  Type,
) where

{-
    "Non-Plutarch Prelude"

    This Prelude is necessary because the on-chain side has an alternative
    prelude that brings into space the `Type` kind, along with many other
    Plutarch functions and types. Because of this single export, we have to
    do the same on the off-chain side to have compatible source files between
    one side and the other
-}

import Data.Kind (Type)
