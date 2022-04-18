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

import BondedPool (hbondedPoolValidator)
import Control.Monad (when)
import Data.Aeson (encode, toJSON)
import Data.ByteString.Lazy qualified as LBS
import NFT (hbondedStakingNFTPolicy)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ParserInfo,
  ReadM,
  argument,
  auto,
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
  strArgument,
  subparser,
  switch,
  value,
 )
import Plutarch.Api.V1 (mintingPolicySymbol, validatorHash)
import Plutus.V1.Ledger.Api (
  CurrencySymbol,
  MintingPolicy (getMintingPolicy),
  PubKeyHash,
  TokenName (unTokenName),
  Validator (getValidator),
 )
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes))
import Plutus.V1.Ledger.Scripts (Script)
import Plutus.V1.Ledger.Tx (TxOutRef (TxOutRef))
import Plutus.V1.Ledger.TxId (TxId)
import Settings (bondedStakingTokenName)
import Types (BondedPoolParams (BondedPoolParams))

serialisePlutusScript :: FilePath -> Script -> IO ()
serialisePlutusScript filepath script =
  let content = encode $ toJSON script
   in LBS.writeFile filepath content

main :: IO ()
main = do
  args <- execParser opts
  case cliCommand args of
    SerialiseNFT txOutRef -> do
      let policy = hbondedStakingNFTPolicy txOutRef
          cs = mintingPolicySymbol policy
      serialisePlutusScript
        (maybe "nft_policy.json" id $ outPath args)
        (getMintingPolicy policy)
      when (printHash args) $
        printVerbose cs
    SerialiseValidator txOutRef pkh -> do
      let policy = hbondedStakingNFTPolicy txOutRef
          cs = mintingPolicySymbol policy
          validator = hbondedPoolValidator $ BondedPoolParams pkh cs
          vh = validatorHash validator
      serialisePlutusScript
        (maybe "validator.json" id $ outPath args)
        (getValidator validator)
      when (printHash args) $ do
        putStrLn $ "Validator hash: " <> show vh
        printVerbose cs

printVerbose :: CurrencySymbol -> IO ()
printVerbose cs = do
  putStrLn $ "Currency symbol: " <> show cs
  putStrLn $ "Token name: " <> show bondedStakingTokenName
  putStrLn $
    "Token name (hex): "
      <> (show . LedgerBytes . unTokenName $ bondedStakingTokenName)

-- Parsers --
data CLI = CLI
  { outPath :: Maybe FilePath
  , printHash :: Bool
  , cliCommand :: CLICommand
  }

data CLICommand
  = SerialiseNFT TxOutRef
  | SerialiseValidator TxOutRef PubKeyHash

opts :: ParserInfo CLI
opts =
  info
    parser
    ( fullDesc
        <> progDesc "Serialise the NFT policy or the validator"
    )

parser :: Parser CLI
parser =
  CLI
    <$> outOption
    <*> printHashSwitch
    <*> commandParser

commandParser :: Parser CLICommand
commandParser = subparser $ serialiseNFTCommand <> serialiseValidatorCommand

serialiseNFTCommand :: Mod CommandFields CLICommand
serialiseNFTCommand =
  command "nft" $
    info
      (SerialiseNFT <$> txOutRefParser)
      ( fullDesc
          <> progDesc
            "Serialise the NFT minting policy by providing a \
            \UTXO"
      )

serialiseValidatorCommand :: Mod CommandFields CLICommand
serialiseValidatorCommand =
  command "validator" $
    info
      ( SerialiseValidator
          <$> txOutRefParser
          <*> pubKeyHashParser
      )
      ( fullDesc
          <> progDesc
            "Serialise the validator by providing a UTXO (used to obtain the \
            \appropiate minting policy) and the stake pool operator's public \
            \key hash"
      )

pubKeyHashParser :: Parser PubKeyHash
pubKeyHashParser =
  strArgument
    ( metavar "PKH"
        <> help "The public key hash corresponding to the stake pool's wallet"
    )

txOutRefParser :: Parser TxOutRef
txOutRefParser = TxOutRef <$> txIdParser <*> integerParser

txIdParser :: Parser TxId
txIdParser =
  strArgument
    ( metavar "TXID"
        <> help "The transaction id containing the desired output"
    )

integerParser :: Parser Integer
integerParser =
  argument
    (auto :: ReadM Integer)
    ( metavar "IDX"
        <> help "The index of the output"
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
