{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "singularitynet-frontend"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "console"
  , "control"
  , "exceptions"
  , "integers"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "transformers"
  , "uint"
  , "datetime"
  , "math"
  , "now"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
