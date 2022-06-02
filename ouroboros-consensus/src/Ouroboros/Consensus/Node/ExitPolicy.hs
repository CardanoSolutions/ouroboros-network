module Ouroboros.Consensus.Node.ExitPolicy
  ( NodeToNodeClientResult (..)
  , returnPolicy
  -- Re-exports
  , ReturnPolicy
  ) where

import           Ouroboros.Network.ExitPolicy
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client


-- | Result of any of the `node-to-node` mini-protocols.  We ignore all but
-- `chain-sync` results.
--
data NodeToNodeClientResult =
    ChainSyncClientResult ChainSyncClientResult
  | NoClientResult


returnPolicy :: ReturnPolicy NodeToNodeClientResult
returnPolicy NoClientResult = ReconnectDelay 0
returnPolicy (ChainSyncClientResult result) = case result of
  -- TODO: it would be nice to have additional context to predict when we will
  -- be ready to reconnect.
  ForkTooDeep      _ _ourTip _theirTip -> ReconnectDelay 120
  NoMoreIntersection _ourTip _theirTip -> ReconnectDelay 120
  RolledBackPastIntersection 
                   _ _ourTip _theirTip -> ReconnectDelay 180
  -- the outbound-governor asked for hot to warm demotion; it's up to the
  -- governor to decide to promote the peer to hot.
  AskedToTerminate                     -> ReconnectDelay 0
