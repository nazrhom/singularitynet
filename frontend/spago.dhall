{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "singularitynet-frontend"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "effect"
  , "identity"
  , "integers"
  , "js-date"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
