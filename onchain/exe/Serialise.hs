module Main (main) where

{- TODO: Update documentation
  This executable can generate CBOR encodings of the NFT minting policy and
  the BondedPool validator. It takes the necessary arguments from the CLI
  to generate them. The resulting scripts are *fully* applied.

  Some examples:

  cabal exec serialise -- state-nft <Transaction Hash> <Output Index>
  cabal exec serialise -- list-nft <Transaction Hash> <Output Index> <Public Key Hash>
  cabal exec serialise -- validator <Transaction Hash> <Output Index> <Public Key Hash>

  Use -v to print to console the policy/validator hash and -o to choose a
  different output file. The latter option can be used with /dev/stdout to print
  the result to screen.
-}

import Control.Monad (when)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
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
import Plutarch (ClosedTerm, compile)
import StateNFT (pbondedStateNFTPolicyUntyped)

serialisePlutusScript :: Script -> Text
serialisePlutusScript script = encodeToLazyText script

writeScriptToFile :: Text -> FilePath -> Script -> IO ()
writeScriptToFile name filepath script =
  TIO.writeFile filepath $
    "exports._" <> name <> " = {\n"
      <> "\tscript: "
      <> serialisePlutusScript script
      <> ",\n};"

serialiseClosedTerm ::
  forall (s :: PType). ClosedTerm s -> CLI -> String -> String -> IO ()
serialiseClosedTerm closedTerm args name json = do
  let script = compile closedTerm
      hash = scriptHash script
  writeScriptToFile
    (T.pack name)
    (maybe json id $ outPath args)
    script
  when (printHash args) $
    putStr $ "\n" <> name <> " hash (unapplied): " <> show hash

main :: IO ()
main = do
  args <- execParser opts
  case cliCommand args of
    SerialiseStateNFT ->
      serialiseClosedTerm
        pbondedStateNFTPolicyUntyped
        args
        "bondedStateNFT"
        "BondedStateNFT.json"
    SerialiseListNFT ->
      serialiseClosedTerm
        pbondedListNFTPolicyUntyped
        args
        "bondedListNFT"
        "BondedListNFT.json"
    SerialiseBondedValidator ->
      serialiseClosedTerm
        pbondedPoolValidatorUntyped
        args
        "bondedPoolValidator"
        "BondedPoolValidator.json"

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
