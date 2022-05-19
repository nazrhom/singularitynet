module Main (main) where

{- TODO: Update documentation
  This executable can generate CBOR encodings of the NFT minting policy and
  the pool validators. It takes the necessary arguments from the CLI to
  generate them. The resulting scripts are *fully* applied.

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
import ListNFT (plistNFTPolicyUntyped)
import Plutarch (ClosedTerm, compile)
import SingularityNet.Settings (bondedStakingTokenName, unbondedStakingTokenName)
import StateNFT (pstateNFTPolicyUntyped)
import UnbondedStaking.UnbondedPool (punbondedPoolValidatorUntyped)

serialisePlutusScript :: Script -> Text
serialisePlutusScript script = encodeToLazyText script

writeScriptToFile ::
  (FilePath -> Text -> IO ()) ->
  Text ->
  FilePath ->
  Script ->
  IO ()
writeScriptToFile writerFunc name filepath script =
  writerFunc filepath $
    "exports._" <> name <> " = {\n"
      <> "\tscript: "
      <> serialisePlutusScript script
      <> ",\n};\n"

serialiseClosedTerm ::
  forall (s :: PType).
  ClosedTerm s ->
  (FilePath -> Text -> IO ()) ->
  CLI ->
  String ->
  String ->
  IO ()
serialiseClosedTerm closedTerm writerFunc args name json = do
  let script = compile closedTerm
      hash = scriptHash script
  writeScriptToFile
    writerFunc
    (T.pack name)
    (maybe json id $ outPath args)
    script
  when (printHash args) $
    putStr $ "\n" <> name <> " hash (unapplied): " <> show hash

main :: IO ()
main = do
  args <- execParser opts
  case cliCommand args of
    SerialiseStateNFT -> do
      serialiseClosedTerm
        (pstateNFTPolicyUntyped bondedStakingTokenName)
        TIO.writeFile
        args
        "bondedStateNFT"
        "StateNFT.json"
      serialiseClosedTerm
        (pstateNFTPolicyUntyped unbondedStakingTokenName)
        TIO.appendFile
        args
        "unbondedStateNFT"
        "StateNFT.json"
    SerialiseListNFT -> do
      serialiseClosedTerm
        plistNFTPolicyUntyped
        TIO.writeFile
        args
        "bondedListNFT"
        "ListNFT.json"
      serialiseClosedTerm
        plistNFTPolicyUntyped
        TIO.appendFile
        args
        "unbondedListNFT"
        "ListNFT.json"
    SerialiseValidator -> do
      serialiseClosedTerm
        pbondedPoolValidatorUntyped
        TIO.writeFile
        args
        "bondedPoolValidator"
        "PoolValidator.json"
      serialiseClosedTerm
        punbondedPoolValidatorUntyped
        TIO.appendFile
        args
        "unbondedPoolValidator"
        "PoolValidator.json"

-- Parsers --
data CLI = CLI
  { outPath :: Maybe FilePath
  , printHash :: Bool
  , cliCommand :: CLICommand
  }

data CLICommand
  = SerialiseStateNFT
  | SerialiseListNFT
  | SerialiseValidator

opts :: ParserInfo CLI
opts =
  info
    parser
    ( fullDesc
        <> progDesc "Serialise a NFT policy or the pool validators"
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
            "Serialise the NFT minting policy of the pools"
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
      (pure SerialiseValidator)
      ( fullDesc
          <> progDesc
            "Serialise the pool validators"
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
