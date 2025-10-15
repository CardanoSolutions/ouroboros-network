{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | One stop shop for configuring diffusion layer for upstream clients
-- This module contains Cardano specific configuration parameters

module Cardano.Network.Diffusion.Configuration
  ( DefaultNumBootstrapPeers (..)
  , NumberOfBigLedgerPeers (..)
  , defaultNumBootstrapPeers
  , defaultSyncTargets
  , defaultNumberOfBigLedgerPeers
  , defaultChainSyncIdleTimeout
  , defaultBlockFetchConfiguration
  , defaultMiniProtocolParameters
  , TxSubmissionLogicVersion (..)
  , defaultTxSubmissionLogicVersion
  , srvPrefix
    -- * Re-exports
  , ChainSyncIdleTimeout (..)
  , BlockFetchConfiguration (..)
  , MiniProtocolParameters (..)
  ) where

import Cardano.Network.NodeToNode (MiniProtocolParameters (..),
           defaultMiniProtocolParameters)
import Cardano.Network.PeerSelection.Governor.PeerSelectionState
           (NumberOfBigLedgerPeers (..))

import Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..),
           GenesisBlockFetchConfiguration (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..))
import Ouroboros.Network.PeerSelection.RelayAccessPoint (SRVPrefix)
import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncIdleTimeout (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
           (TxSubmissionLogicVersion (..))

-- | Default number of bootstrap peers
--
newtype DefaultNumBootstrapPeers =
  DefaultNumBootstrapPeers { getDefaultNumBootstrapPeers :: Int }
  deriving (Eq, Show)

defaultNumBootstrapPeers :: DefaultNumBootstrapPeers
defaultNumBootstrapPeers = DefaultNumBootstrapPeers 30

-- | These targets are established when Genesis mode is enabled
-- in node configuration and when the node is syncing up
--
defaultSyncTargets :: PeerSelectionTargets
defaultSyncTargets =
  PeerSelectionTargets {
    targetNumberOfRootPeers                 = 0,
    targetNumberOfKnownPeers                = 150,
    targetNumberOfEstablishedPeers          = 10,
    targetNumberOfActivePeers               = 5,
    targetNumberOfKnownBigLedgerPeers       = 100,
    targetNumberOfEstablishedBigLedgerPeers = 40,
    targetNumberOfActiveBigLedgerPeers      = 30 }

-- | This parameter controls the minimum number of active connections
--   with big ledger peers that must be maintained when syncing in
--   Genesis mode such that trusted state can be signalled to Consensus.
--   Exiting syncing / entering deadline mode is predicated on this
--   condition. This should be below `targetNumberOfActiveBigLedgerPeers`
--   in `syncTargets` otherwise untrusted state will never be departed.
--   This value is lower than the target, because in Genesis we may
--   demote a big ledger peer for underperformance and not immediately
--   promote one from the warm set in case there are adversaries
--   whom are intentionally trying to slow us down.
--
defaultNumberOfBigLedgerPeers :: NumberOfBigLedgerPeers
defaultNumberOfBigLedgerPeers = NumberOfBigLedgerPeers 5

defaultChainSyncIdleTimeout :: ChainSyncIdleTimeout
defaultChainSyncIdleTimeout = ChainSyncIdleTimeout 3373

srvPrefix :: SRVPrefix
srvPrefix = "_cardano._tcp"


-- | Configuration for FetchDecisionPolicy.
--
defaultBlockFetchConfiguration :: Int -> BlockFetchConfiguration
defaultBlockFetchConfiguration bfcSalt =
  BlockFetchConfiguration {
    bfcMaxConcurrencyBulkSync = 1,
    bfcMaxConcurrencyDeadline = 1,
    bfcMaxRequestsInflight    = fromIntegral $ blockFetchPipeliningMax defaultMiniProtocolParameters,
    bfcDecisionLoopIntervalGenesis = 0.04,  -- 40ms
    bfcDecisionLoopIntervalPraos = 0.01,  -- 10ms
    bfcGenesisBFConfig        = GenesisBlockFetchConfiguration
      { gbfcGracePeriod = 10 },  -- seconds
    bfcSalt }

-- | The default logic version is the legacy one, the new one is still
-- experimental.
defaultTxSubmissionLogicVersion :: TxSubmissionLogicVersion
defaultTxSubmissionLogicVersion = TxSubmissionLogicV1
