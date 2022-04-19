{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "singularitynet-frontend"
, dependencies = ["aff", "cardano-transaction-lib"]
, packages = ./packages.dhall
, sources = [ "exe/**/*.purs", "test/**/*.purs" ]
}
