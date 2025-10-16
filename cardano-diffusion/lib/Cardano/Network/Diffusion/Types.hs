{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Network.Diffusion.Types
  ( CardanoNodeArguments (..)
  , CardanoConsensusArguments (..)
  , CardanoTracers
  , Diffusion.Tracers (..)
  , Diffusion.nullTracers
  , CardanoConfiguration
  , CardanoApplications
  , Diffusion.Configuration (..)
  , Diffusion.Applications (..)
  , CardanoPeerSelectionCounters
  , CardanoLocalRootConfig
  , CardanoTraceLocalRootPeers
  , CardanoTracePeerSelection
  , CardanoDebugPeerSelection
    -- * Re-exports
  , Cardano.Churn.TraceChurnMode (..)
  , module Reexports
  ) where


import Control.Concurrent.Class.MonadSTM.Strict
import Control.Tracer (Tracer)
import Network.Socket (SockAddr, Socket)

import Cardano.Network.ConsensusMode as Reexports (ConsensusMode (..))
import Cardano.Network.LedgerPeerConsensusInterface qualified as Cardano
import Cardano.Network.NodeToClient (LocalAddress, LocalSocket,
           NodeToClientVersion, NodeToClientVersionData)
import Cardano.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)
import Cardano.Network.PeerSelection as Rexports (NumberOfBigLedgerPeers (..),
           PeerTrustable (..), UseBootstrapPeers (..))
import Cardano.Network.PeerSelection qualified as Cardano
import Cardano.Network.PeerSelection.Churn qualified as Cardano.Churn

import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.PeerSelection as Reexports
           (LedgerPeersConsensusInterface (..), PeerMetrics,
           PeerSelectionTargets)

-- | Arguments required to instantiate Cardano Node Diffusion
--
data CardanoNodeArguments m = CardanoNodeArguments {
    consensusMode          :: ConsensusMode,
    -- ^ Field which comes from `cardano-node` configuration file
    -- (`ncConsensusMode`).
    genesisPeerSelectionTargets
                           :: PeerSelectionTargets,
    -- ^ Fields which come from `cardano-node` configuration file.
    minNumOfBigLedgerPeers :: NumberOfBigLedgerPeers,
    -- ^ Field which comes from `cardano-node` configuration file.
    tracerChurnMode        :: Tracer m Cardano.Churn.TraceChurnMode
    -- ^ Field which comes from `cardano-node` tracing system.
  }

-- | Arguments required to instantiate Cardano Node Diffusion.
--
data CardanoConsensusArguments ntnAddr m =
  CardanoConsensusArguments {
    churnModeVar           :: StrictTVar m Cardano.Churn.ChurnMode,
    -- ^ churn mode var is created in `ouroboros-consensus-diffusion` and shared
    -- with diffusion and peer selection policy (instantiated in
    -- `ouroboros-consensus-diffusion):
    --
    -- * peer selection is updating it;
    -- * peer selection policy is reading it.

    churnMetrics           :: PeerMetrics m ntnAddr,
    -- ^ churn metrics are used in cardano diffusion by `SIGUSR1` handler; in
    -- consensus they are passed to
    --
    -- * applications (e.g. `chain-sync` and `block-fetch`), where they are
    --   updated;
    -- * peer selection policy, where they are read.

    ledgerPeersAPI         :: LedgerPeersConsensusInterface (Cardano.LedgerPeersConsensusInterface m) m,
    -- ^ ledger and consensus APIs

    readUseBootstrapPeers  :: STM m UseBootstrapPeers
    -- ^ `UseBootstrapPeers` from topology file.
    --
    -- `readUseBootstrapPeers` is created in `cardano-node`, then passed to
    -- consensus through `RunNodeArgs`, from which it is passed to diffusion.
  }

type CardanoTracers m =
  Diffusion.Tracers
    RemoteAddress NodeToNodeVersion  NodeToNodeVersionData
    LocalAddress  NodeToClientVersion NodeToClientVersionData
    Cardano.ExtraState
    Cardano.DebugPeerSelectionState
    PeerTrustable
    (Cardano.ExtraPeers RemoteAddress)
    (Cardano.ExtraPeerSelectionSetsWithSizes RemoteAddress)
    Cardano.ExtraTrace
    m


type CardanoConfiguration m =
  Diffusion.Configuration
    PeerTrustable
    m
    Socket
    RemoteAddress
    LocalSocket
    LocalAddress


type CardanoApplications m a =
  Diffusion.Applications
    RemoteAddress
    NodeToNodeVersion
    NodeToNodeVersionData
    LocalAddress
    NodeToClientVersion
    NodeToClientVersionData
    m a



type CardanoLocalRootConfig = Cardano.LocalRootConfig PeerTrustable


type CardanoTraceLocalRootPeers =
  Cardano.TraceLocalRootPeers PeerTrustable RemoteAddress


type CardanoTracePeerSelection =
  Cardano.TracePeerSelection Cardano.DebugPeerSelectionState
                             PeerTrustable
                             (Cardano.ExtraPeers SockAddr)
                             RemoteAddress


type CardanoDebugPeerSelection =
  Cardano.DebugPeerSelection Cardano.ExtraState
                             PeerTrustable
                             (Cardano.ExtraPeers RemoteAddress)
                             RemoteAddress


type CardanoPeerSelectionCounters =
  Cardano.PeerSelectionCounters (Cardano.ExtraPeerSelectionSetsWithSizes RemoteAddress)
