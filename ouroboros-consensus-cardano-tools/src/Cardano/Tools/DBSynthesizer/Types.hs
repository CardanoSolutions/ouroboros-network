
module Cardano.Tools.DBSynthesizer.Types (module Cardano.Tools.DBSynthesizer.Types) where

import           Data.Aeson as Aeson (Value)
import           Data.Word (Word64)
import           Ouroboros.Consensus.Block.Abstract (SlotNo (..))


data NodeConfigStub =
    NodeConfigStub {
        ncsNodeConfig         :: !Aeson.Value
      , ncsAlonzoGenesisFile  :: !FilePath
      , ncsShelleyGenesisFile :: !FilePath
      , ncsByronGenesisFile   :: !FilePath
    }
    deriving Show

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
