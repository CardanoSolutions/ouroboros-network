{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Ouroboros.Network.PeerSelection.PeerMetric where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer (..), contramap, nullTracer)
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as Pq
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.DeltaQ (SizeInBytes)
import           Ouroboros.Network.NodeToNode (ConnectionId (..))
import           Ouroboros.Network.PeerSelection.PeerMetric.Type


-- The maximum numbers of slots we will store data for.
-- On some chains sometimes this corresponds to 1h
-- worth of metrics *sighs*.
maxEntriesToTrack :: Int
maxEntriesToTrack = 180


type SlotMetric p = IntPSQ SlotNo (p, Time)

data PeerMetrics m p = PeerMetrics {
    headerMetrics  :: StrictTVar m (SlotMetric p)
  , fetchedMetrics :: StrictTVar m (SlotMetric (p, SizeInBytes))
  }

reportMetric
    :: forall m p.
       ( MonadSTM m )
     => PeerMetrics m p
     -> ReportPeerMetrics m (ConnectionId p)
reportMetric peerMetrics =
  ReportPeerMetrics (headerMetricTracer peerMetrics)
                    (fetchedMetricTracer peerMetrics)

nullMetric
    :: MonadSTM m
    => ReportPeerMetrics m p
nullMetric =
  ReportPeerMetrics nullTracer nullTracer

slotMetricKey :: SlotNo -> Int
slotMetricKey (SlotNo s) = fromIntegral s

headerMetricTracer
    :: forall m p.
       ( MonadSTM m )
    => PeerMetrics m p
    -> Tracer (STM m) (TraceLabelPeer (ConnectionId p) (SlotNo, Time))
headerMetricTracer PeerMetrics{headerMetrics} =
    (\(TraceLabelPeer con d) -> TraceLabelPeer (remoteAddress con) d)
    `contramap`
    metricsTracer headerMetrics

fetchedMetricTracer
    :: forall m p.
       ( MonadSTM m )
    => PeerMetrics m p
    -> Tracer (STM m) (TraceLabelPeer (ConnectionId p)
                                      ( SizeInBytes
                                      , SlotNo
                                      , Time
                                      ))
fetchedMetricTracer PeerMetrics{fetchedMetrics} =
    (\(TraceLabelPeer con (bytes, slot, time)) ->
       TraceLabelPeer (remoteAddress con, bytes) (slot, time))
    `contramap`
     metricsTracer fetchedMetrics


getHeaderMetrics
    :: MonadSTM m
    => PeerMetrics m p
    -> STM m (SlotMetric p)
getHeaderMetrics PeerMetrics{headerMetrics} = readTVar headerMetrics

getFetchedMetrics
    :: MonadSTM m
    => PeerMetrics m p
    -> STM m (SlotMetric (p, SizeInBytes))
getFetchedMetrics PeerMetrics{fetchedMetrics} = readTVar fetchedMetrics

metricsTracer
    :: forall m p.  ( MonadSTM m )
    => StrictTVar m (SlotMetric p)
    -> Tracer (STM m) (TraceLabelPeer p (SlotNo, Time))
metricsTracer metricsVar = Tracer $ \(TraceLabelPeer !peer (!slot, !time)) -> do
    metrics <- readTVar metricsVar
    case Pq.lookup (slotMetricKey slot) metrics of
         Nothing -> do
             let metrics' = Pq.insert (slotMetricKey slot) slot (peer, time) metrics
             if Pq.size metrics' > maxEntriesToTrack
                then
                  case Pq.minView metrics' of
                       Nothing -> error "impossible empty pq" -- We just inserted an element!
                       Just (_, minSlotNo, _, metrics'') ->
                            if minSlotNo == slot
                               then return ()
                               else writeTVar metricsVar metrics''
             else writeTVar metricsVar metrics'
         Just (_, (_, oldTime)) ->
             if oldTime <= time
                then return ()
                else writeTVar metricsVar (Pq.insert (slotMetricKey slot) slot (peer, time) metrics)

newPeerMetric
    :: MonadSTM m
    => m (PeerMetrics m p)
newPeerMetric = do
  hs <- newTVarIO Pq.empty
  bs <- newTVarIO Pq.empty
  return $ PeerMetrics hs bs

-- Returns a Map which counts the number of times a given peer
-- was the first to present us with a block/header.
upstreamyness
    :: forall p.  ( Ord p )
    => SlotMetric p
    -> Map p Int
upstreamyness = Pq.fold' count Map.empty
  where
    count :: Int
          -> SlotNo
          -> (p,Time)
          -> Map p Int
          -> Map p Int
    count _ _ (peer,_) m =
        Map.alter fn peer m
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing  = Just 1
        fn (Just c) = Just $! c + 1


-- Returns a Map which counts the number of bytes downloaded
-- for a given peer.
fetchynessBytes
    :: forall p.  ( Ord p )
    => SlotMetric (p, SizeInBytes)
    -> Map p Int
fetchynessBytes = Pq.fold' count Map.empty
  where
    count :: Int
          -> SlotNo
          -> ((p, SizeInBytes), Time)
          -> Map p Int
          -> Map p Int
    count _ _ ((peer, bytes),_) m =
        Map.alter fn peer m
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing         = Just $ fromIntegral bytes
        fn (Just oldBytes) = Just $! oldBytes + fromIntegral bytes

-- Returns a Map which counts the number of times a given peer
-- was the first we downloaded a block from.
fetchynessBlocks
    :: forall p.  ( Ord p )
    => SlotMetric (p, SizeInBytes)
    -> Map p Int
fetchynessBlocks = Pq.fold' count Map.empty
  where
    count :: Int
          -> SlotNo
          -> ((p, SizeInBytes), Time)
          -> Map p Int
          -> Map p Int
    count _ _ ((peer, _),_) m =
        Map.alter fn peer m
      where
        fn :: Maybe Int -> Maybe Int
        fn Nothing  = Just 1
        fn (Just c) = Just $! c + 1


