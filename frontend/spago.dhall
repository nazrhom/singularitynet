{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "singularitynet-frontend"
, dependencies =
  [ "aff"
  , "cardano-transaction-lib"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "bigints"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "rationals"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
