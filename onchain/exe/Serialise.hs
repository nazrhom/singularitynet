module Main (main) where

{-
  This executable can generate CBOR encodings of the NFT minting policy and
  the BondedPool validator. It takes the necessary arguments from the CLI
  to generate them. The resulting scripts are *fully* applied.

  Some examples:

  cabal exec serialise -- nft <Transaction Hash> <Output Index>
  cabal exec serialise -- validator <Transaction Hash> <Output Index> <Public Key Hash>

  Use -v to print to console the policy/validator hash and -o to choose a
  different output file. The latter option can be used with /dev/stdout to print
  the result to screen.
-}

import Control.Monad (when)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO qualified as TIO
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ParserInfo,
  command,
  execParser,
  fullDesc,
  help,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  showDefault,
  str,
  subparser,
  switch,
  value,
 )
import Plutarch.Api.V1 (scriptHash)
import Plutus.V1.Ledger.Scripts (Script)

import BondedPool (pbondedPoolValidatorUntyped)
import ListNFT (pbondedListNFTPolicyUntyped)
import Plutarch (compile)
import StateNFT (pbondedStateNFTPolicyUntyped)

serialisePlutusScript :: Script -> Text
serialisePlutusScript script = encodeToLazyText script

writeScriptToFile :: Text -> FilePath -> Script -> IO ()
writeScriptToFile name filepath script =
  TIO.writeFile filepath $
    "exports._" <> name <> " = {\n"
      <> "\tvalidator: "
      <> serialisePlutusScript script
      <> ",\n};"

main :: IO ()
main = do
  args <- execParser opts
  case cliCommand args of
    SerialiseStateNFT -> do
      let script = compile pbondedStateNFTPolicyUntyped
          hash = scriptHash script
      writeScriptToFile
        "bondedStateNFT"
        (maybe "BondedStateNFT.json" id $ outPath args)
        script
      when (printHash args) $
        putStr $ "\nstateNFT hash (unapplied): " <> show hash
    SerialiseListNFT -> do
      let script = compile pbondedListNFTPolicyUntyped
          hash = scriptHash script
      writeScriptToFile
        "bondedListNFT"
        (maybe "BondedListNFT.json" id $ outPath args)
        script
      when (printHash args) $
        putStr $ "\nlistNFT hash (unapplied): " <> show hash
    SerialiseBondedValidator -> do
      let script = compile pbondedPoolValidatorUntyped
          hash = scriptHash script
      writeScriptToFile
        "bondedPoolValidator"
        (maybe "BondedPoolValidator.json" id $ outPath args)
        script
      when (printHash args) $
        putStr $ "\npoolValidator hash (unapplied): " <> show hash

-- Parsers --
data CLI = CLI
  { outPath :: Maybe FilePath
  , printHash :: Bool
  , cliCommand :: CLICommand
  }

data CLICommand
  = SerialiseStateNFT
  | SerialiseListNFT
  | SerialiseBondedValidator

opts :: ParserInfo CLI
opts =
  info
    parser
    ( fullDesc
        <> progDesc "Serialise a NFT policy or the bonded pool's validator"
    )

parser :: Parser CLI
parser =
  CLI
    <$> outOption
    <*> printHashSwitch
    <*> commandParser

commandParser :: Parser CLICommand
commandParser =
  subparser $
    serialiseStateNFTCommand
      <> serialiseListNFTCommand
      <> serialiseValidatorCommand

serialiseStateNFTCommand :: Mod CommandFields CLICommand
serialiseStateNFTCommand =
  command "state-policy" $
    info
      (pure SerialiseStateNFT)
      ( fullDesc
          <> progDesc
            "Serialise the NFT minting policy of the bonded pool"
      )

serialiseListNFTCommand :: Mod CommandFields CLICommand
serialiseListNFTCommand =
  command "list-policy" $
    info
      (pure SerialiseListNFT)
      ( fullDesc
          <> progDesc
            "Serialise the NFT minting policy of the association list"
      )

serialiseValidatorCommand :: Mod CommandFields CLICommand
serialiseValidatorCommand =
  command "validator" $
    info
      (pure SerialiseBondedValidator)
      ( fullDesc
          <> progDesc
            "Serialise the bonded pool validator"
      )

outOption :: Parser (Maybe FilePath)
outOption =
  option
    (Just <$> str)
    ( long "out"
        <> short 'o'
        <> metavar "OUT"
        <> help "Location of serialised file"
        <> value Nothing
        <> showDefault
    )

printHashSwitch :: Parser Bool
printHashSwitch =
  switch
    ( long "print-hash"
        <> short 'v'
    )
