module Main (main) where

{-
  This executable can generate CBOR encodings of the NFT minting policy and
  the BondedPool validator. It takes the necessary arguments from the CLI
  to generate them. The resulting scripts are *fully* applied.

-}

import BondedPool (hbondedPoolValidator)
import Cardano.Binary qualified as CBOR
import Codec.Serialise (serialise)
import Data.Aeson (KeyValue ((.=)), encode, object)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Text.Encoding qualified as Text
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
  progDesc,
  short,
  showDefault,
  strArgument,
  strOption,
  subparser,
  value,
 )
import Plutarch.Api.V1 (scriptHash)
import Plutus.V1.Ledger.Api (CurrencySymbol (CurrencySymbol), PubKeyHash)
import Plutus.V1.Ledger.Scripts (Script, getScriptHash, scriptSize)
import Plutus.V1.Ledger.Tx (TxOutRef (TxOutRef))
import Plutus.V1.Ledger.TxId (TxId)
import Types (BondedPoolParams (BondedPoolParams))

serialisePlutusScript :: String -> FilePath -> Script -> IO ()
serialisePlutusScript title filepath scrpt = do
  let scriptSBS = SBS.toShort . LBS.toStrict . serialise $ scrpt
      scriptRawCBOR = CBOR.serialize' scriptSBS
      scriptType = "PlutusScriptV1" :: String
      plutusJson =
        object
          [ "type" .= scriptType
          , "description" .= title
          , "cborHex" .= Text.decodeUtf8 (Base16.encode scriptRawCBOR)
          ]
      content = encode plutusJson
  print title >> putStr "Script size " >> print (scriptSize scrpt)
  LBS.writeFile filepath content

main :: IO ()
main = do
  args <- execParser opts
  pure ()
  case args of
    SerialiseNFT out txOutRef ->
      let policy = hbondedStakingNFTPolicy txOutRef
          policyHash = scriptHash policy
       in do
            serialisePlutusScript
              "SingularityNet NFT Policy - Applied"
              out
              policy
            putStrLn $ "Policy hash (Currency Symbol): " <> show policyHash
    SerialiseValidator out txOutRef pkh ->
      let policy = hbondedStakingNFTPolicy txOutRef
          cs = CurrencySymbol . getScriptHash . scriptHash $ policy
          validator = hbondedPoolValidator $ BondedPoolParams pkh cs
          validatorHash = scriptHash validator
       in do
            serialisePlutusScript
              "SingularityNet Bonded Pool Validator - Applied"
              out
              validator
            putStrLn $ "Validator hash: " <> show validatorHash

-- Parsers --
data CLI
  = SerialiseNFT FilePath TxOutRef
  | SerialiseValidator FilePath TxOutRef PubKeyHash

opts :: ParserInfo CLI
opts =
  info
    (subparser $ serialiseNFTCommand <> serialiseValidatorCommand)
    ( fullDesc
        <> progDesc "Serialise the NFT minting policy or the validator"
    )

serialiseNFTCommand :: Mod CommandFields CLI
serialiseNFTCommand =
  command "nft" $
    info
      (SerialiseNFT <$> outOption "nft-policy.json" <*> txOutRefParser)
      (fullDesc <> progDesc "Serialise the NFT minting policy by providing a UTXO")

serialiseValidatorCommand :: Mod CommandFields CLI
serialiseValidatorCommand =
  command "validator" $
    info
      ( SerialiseValidator
          <$> outOption "validator.json"
          <*> txOutRefParser
          <*> pubKeyHashParser
      )
      ( fullDesc
          <> progDesc
            "Serialise the validator by providing a UTXO (used to \
            \ obtain the appropiate minting policy) and the stake pool operator's public \
            \ key hash"
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

outOption :: FilePath -> Parser FilePath
outOption def =
  strOption
    ( long "out"
        <> short 'o'
        <> metavar "OUT"
        <> help "Location of serialised file"
        <> value def
        <> showDefault
    )
