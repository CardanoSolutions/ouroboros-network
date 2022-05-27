

module Configuration.Parsers (
    ForgeOptions (..)
  , NodeCredentials (..)
  , NodeFilePaths (..)
  , parserCommandLine
  ) where

import           Data.Word (Word64)

import           Options.Applicative
import           Ouroboros.Consensus.Block.Abstract (SlotNo (..))


data NodeFilePaths =
    NodeFilePaths {
        nfpConfig  :: !FilePath
      , nfpChainDB :: !FilePath
    }
    deriving Show

data NodeCredentials =
    NodeCredentials {
        credCertFile :: !(Maybe FilePath)
      , credVRFFile  :: !(Maybe FilePath)
      , credKESFile  :: !(Maybe FilePath)
      , credBulkFile :: !(Maybe FilePath)
    }
    deriving Show

data ForgeOptions =
    ForgeLimitBlock !Word64
  | ForgeLimitSlot  !SlotNo
  | ForgeLimitEpoch !Word64
    deriving Show


parserCommandLine :: Parser (NodeFilePaths, NodeCredentials, ForgeOptions)
parserCommandLine =
  (,,)
    <$> parseNodeFilePaths
    <*> parseNodeCredentials
    <*> parseForgeOptions

parseNodeFilePaths :: Parser NodeFilePaths
parseNodeFilePaths =
  NodeFilePaths
    <$> parseNodeConfigFilePath
    <*> parseChainDBFilePath

parseNodeCredentials :: Parser NodeCredentials
parseNodeCredentials =
  NodeCredentials
    <$> optional parseOperationalCertFilePath
    <*> optional parseVrfKeyFilePath
    <*> optional parseKesKeyFilePath
    <*> optional parseBulkFilePath

parseForgeOptions :: Parser ForgeOptions
parseForgeOptions =
      ForgeLimitSlot <$> parseSlotLimit
  <|> ForgeLimitBlock <$> parseBlockLimit
  <|> ForgeLimitEpoch <$> parseEpochLimit

parseChainDBFilePath :: Parser FilePath
parseChainDBFilePath =
  strOption
    ( long "db"
        <> metavar "PATH"
        <> help "Path to the Chain DB"
        <> completer (bashCompleter "directory")
    )

parseNodeConfigFilePath :: Parser FilePath
parseNodeConfigFilePath =
  strOption
    ( long "node-config"
        <> metavar "FILE"
        <> help "Path to the node's config.json"
        <> completer (bashCompleter "file")
    )

parseOperationalCertFilePath :: Parser FilePath
parseOperationalCertFilePath =
  strOption
    ( long "shelley-operational-certificate"
        <> metavar "FILE"
        <> help "Path to the delegation certificate"
        <> completer (bashCompleter "file")
    )

parseKesKeyFilePath :: Parser FilePath
parseKesKeyFilePath =
  strOption
    ( long "shelley-kes-key"
        <> metavar "FILE"
        <> help "Path to the KES signing key"
        <> completer (bashCompleter "file")
    )

parseVrfKeyFilePath :: Parser FilePath
parseVrfKeyFilePath =
  strOption
    ( long "shelley-vrf-key"
        <> metavar "FILE"
        <> help "Path to the VRF signing key"
        <> completer (bashCompleter "file")
    )

parseBulkFilePath :: Parser FilePath
parseBulkFilePath =
  strOption
    ( long "bulk-credentials-file"
        <> metavar "FILE"
        <> help "Path to the bulk credentials file"
        <> completer (bashCompleter "file")
    )

parseSlotLimit :: Parser SlotNo
parseSlotLimit =
  SlotNo <$> option auto
    (     short 's'
       <> long "slots"
       <> metavar "NUMBER"
       <> help "Amount of slots to process"
    )

parseBlockLimit :: Parser Word64
parseBlockLimit =
  option auto
    (     short 'b'
       <> long "blocks"
       <> metavar "NUMBER"
       <> help "Amount of blocks to forge"
    )

parseEpochLimit :: Parser Word64
parseEpochLimit =
  option auto
    (     short 'e'
       <> long "epochs"
       <> metavar "NUMBER"
       <> help "Amount of epochs to process"
    )
