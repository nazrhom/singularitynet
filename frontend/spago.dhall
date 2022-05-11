{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "singularitynet-frontend"
, dependencies =
  [ "aff"
  , "cardano-transaction-lib"
  , "argonaut"
  , "arrays"
  , "bigints"
  , "integers"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "tuples"
  , "identity"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
