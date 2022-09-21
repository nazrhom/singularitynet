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
  , "bifunctors"
  , "cardano-transaction-lib"
  , "console"
  , "control"
  , "exceptions"
  , "effect"
  , "integers"
  , "maybe"
  , "monad-logger"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "record"
  , "strings"
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
