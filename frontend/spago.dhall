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
  , "effect"
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
  , "optparse"
  -- The project does not compile if these CTL dependencies are not added
  , "purescript-toppokki"
  , "monad-logger"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
