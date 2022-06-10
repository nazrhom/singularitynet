{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "singularitynet-frontend"
, dependencies =
  [ "aeson"
  , "aeson-helpers"
  , "aff"
  , "aff-promise"
  , "argonaut-codecs"
  , "argonaut-core"
  , "cardano-transaction-lib"
  , "control"
  , "arrays"
  , "bigints"
  , "exceptions"
  , "integers"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "text-encoding"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
