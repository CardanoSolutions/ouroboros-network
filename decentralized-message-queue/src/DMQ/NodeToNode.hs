{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.NodeToNode where


import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (NFData)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, nullTracer)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Data.ByteString.Lazy qualified as BL
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Generics (Generic)
import System.Random (mkStdGen)

import Network.Mux.Trace qualified as Mx
import Network.Mux.Types (Mode (..))
import Network.Mux.Types qualified as Mx
import Network.TypedProtocol.Codec (Codec)

import DMQ.Diffusion.NodeKernel (NodeKernel (..))
import DMQ.Protocol.SigSubmission.Codec
import DMQ.Protocol.SigSubmission.Type

-- TODO: remove this dependency
import Cardano.Network.NodeToNode (addSafetyMargin, keepAliveMiniProtocolNum,
           peerSharingMiniProtocolNum)

import Ouroboros.Network.BlockFetch.ClientRegistry (bracketKeepAliveClient)
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.CodecCBORTerm (CodecCBORTerm (..))
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ConnectionManager.Types (DataFlow (..))
import Ouroboros.Network.Context (ExpandedInitiatorContext (..),
           ResponderContext (..))
import Ouroboros.Network.Driver.Limits (runPeerWithLimits,
           runPipelinedPeerWithLimits)
import Ouroboros.Network.Driver.Simple (TraceSendRecv)
import Ouroboros.Network.Handshake.Acceptable (Accept (..), Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.KeepAlive (KeepAliveInterval (..), keepAliveClient,
           keepAliveServer)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..),
           MiniProtocolLimits (..), OuroborosBundle,
           OuroborosBundleWithExpandedCtx, RunMiniProtocol (..),
           StartOnDemandOrEagerly (..), TemperatureBundle (..),
           WithProtocolTemperature (..))
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.NodeToNode.Version qualified as NTN
import Ouroboros.Network.PeerSelection (PeerSharing (..))
import Ouroboros.Network.PeerSharing (bracketPeerSharingClient,
           peerSharingClient, peerSharingServer)
import Ouroboros.Network.TxSubmission.Inbound.V2 as SigSubmission
import Ouroboros.Network.TxSubmission.Mempool.Simple qualified as Mempool
import Ouroboros.Network.TxSubmission.Outbound

import Ouroboros.Network.Protocol.Handshake (Handshake, HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
           codecHandshake, timeLimitsHandshake)
import Ouroboros.Network.Protocol.KeepAlive.Client (keepAliveClientPeer)
import Ouroboros.Network.Protocol.KeepAlive.Codec (byteLimitsKeepAlive,
           codecKeepAlive_v2, timeLimitsKeepAlive)
import Ouroboros.Network.Protocol.KeepAlive.Server (keepAliveServerPeer)
import Ouroboros.Network.Protocol.KeepAlive.Type (KeepAlive)
import Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits,
           ProtocolTimeLimits)
import Ouroboros.Network.Protocol.PeerSharing.Client (peerSharingClientPeer)
import Ouroboros.Network.Protocol.PeerSharing.Codec (byteLimitsPeerSharing,
           codecPeerSharing, timeLimitsPeerSharing)
import Ouroboros.Network.Protocol.PeerSharing.Server (peerSharingServerPeer)
import Ouroboros.Network.Protocol.PeerSharing.Type qualified as Protocol
import Ouroboros.Network.Protocol.TxSubmission2.Client (txSubmissionClientPeer)
import Ouroboros.Network.Protocol.TxSubmission2.Server
           (txSubmissionServerPeerPipelined)


data NodeToNodeVersion =
  NodeToNodeV_1
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, NFData)

nodeToNodeVersionCodec :: CodecCBORTerm (Text, Maybe Int) NodeToNodeVersion
nodeToNodeVersionCodec = CodecCBORTerm { encodeTerm, decodeTerm }
  where
    encodeTerm NodeToNodeV_1 = CBOR.TInt 1

    decodeTerm (CBOR.TInt 1) = Right NodeToNodeV_1
    decodeTerm (CBOR.TInt n) = Left ( T.pack "decode NodeToNodeVersion: unknown tag: "
                                        <> T.pack (show n)
                                    , Just n
                                    )
    decodeTerm _ = Left ( T.pack "decode NodeToNodeVersion: unexpected term"
                        , Nothing)

-- | Version data for NodeToNode protocols
--
-- This data type is inpired by the one defined in 'ouroboros-network-api',
-- however, it is redefined here to tie it to our custom `NodeToNodeVersion`
-- and to avoid divergences.
--
data NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic  :: !NetworkMagic
  , diffusionMode :: !DiffusionMode
  , peerSharing   :: !PeerSharing
  , query         :: !Bool
  }
  deriving (Show, Eq)

instance Acceptable NodeToNodeVersionData where
    -- | Check that both side use the same 'networkMagic'.  Choose smaller one
    -- from both 'diffusionMode's, e.g. if one is running in 'InitiatorOnlyMode'
    -- agree on it. Agree on the same 'PeerSharing' value, if the negotiated
    -- diffusion mode is 'InitiatorAndResponder', otherwise default to
    -- 'PeerSharingDisabled'.
    acceptableVersion local remote
      | networkMagic local == networkMagic remote
      = let acceptedDiffusionMode = diffusionMode local `min` diffusionMode remote
         in Accept NodeToNodeVersionData
              { networkMagic  = networkMagic local
              , diffusionMode = acceptedDiffusionMode
              , peerSharing   = case acceptedDiffusionMode of
                                  InitiatorAndResponderDiffusionMode ->
                                    peerSharing local <> peerSharing remote
                                  InitiatorOnlyDiffusionMode         ->
                                    PeerSharingDisabled
              , query         = query local || query remote
              }
      | otherwise
      = Refuse $ T.pack $ "version data mismatch: "
                       ++ show local
                       ++ " /= " ++ show remote

instance Queryable NodeToNodeVersionData where
    queryVersion = query

nodeToNodeCodecCBORTerm :: NodeToNodeVersion
                        -> CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm =
  \case
    NodeToNodeV_1 -> v1

  where
    v1 = CodecCBORTerm { encodeTerm = encodeTerm1, decodeTerm = decodeTerm1 }

    encodeTerm1 :: NodeToNodeVersionData -> CBOR.Term
    encodeTerm1 NodeToNodeVersionData { networkMagic, diffusionMode, peerSharing, query }
      = CBOR.TList
          [ CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)
          , CBOR.TBool (case diffusionMode of
                         InitiatorOnlyDiffusionMode         -> True
                         InitiatorAndResponderDiffusionMode -> False)
          , CBOR.TInt (case peerSharing of
                         PeerSharingDisabled -> 0
                         PeerSharingEnabled  -> 1)
          , CBOR.TBool query
          ]

    decodeTerm1 :: CBOR.Term -> Either Text NodeToNodeVersionData
    decodeTerm1 (CBOR.TList [CBOR.TInt x, CBOR.TBool diffusionMode, CBOR.TInt peerSharing, CBOR.TBool query])
      | x >= 0
      , x <= 0xffffffff
      , Just ps <- case peerSharing of
                    0 -> Just PeerSharingDisabled
                    1 -> Just PeerSharingEnabled
                    _ -> Nothing
      = Right
          NodeToNodeVersionData {
              networkMagic = NetworkMagic (fromIntegral x),
              diffusionMode = if diffusionMode
                              then InitiatorOnlyDiffusionMode
                              else InitiatorAndResponderDiffusionMode,
              peerSharing = ps,
              query = query
            }
      | x < 0 || x > 0xffffffff
      = Left $ T.pack $ "networkMagic out of bound: " <> show x
      | otherwise -- peerSharing < 0 || peerSharing > 1
      = Left $ T.pack $ "peerSharing is out of bound: " <> show peerSharing
    decodeTerm1 t
      = Left $ T.pack $ "unknown encoding: " ++ show t

ntnDataFlow :: NodeToNodeVersionData -> DataFlow
ntnDataFlow NodeToNodeVersionData { diffusionMode } =
  case diffusionMode of
    InitiatorAndResponderDiffusionMode -> Duplex
    InitiatorOnlyDiffusionMode         -> Unidirectional

-- | Map between DMQ NTNVersion and Ouroboros NTNVersion
--
-- Useful for reusing codecs and other functions
--
mapNtNDMQtoOuroboros :: NodeToNodeVersion -> NTN.NodeToNodeVersion
mapNtNDMQtoOuroboros _ = maxBound

-- TODO: if we add `versionNumber` to `ctx` we could use `RunMiniProtocolCb`.
-- This makes sense, since `ctx` already contains `versionData`.
type ClientApp addr m a =
     NodeToNodeVersion
  -> ExpandedInitiatorContext addr m
  -> Channel m BL.ByteString
  -> m (a, Maybe BL.ByteString)

type ServerApp addr m a =
     NodeToNodeVersion
  -> ResponderContext addr
  -> Channel m BL.ByteString
  -> m (a, Maybe BL.ByteString)

data Apps addr m a b =
  Apps {
    -- | Start a sig-submission client
    aSigSubmissionClient :: ClientApp addr m a

    -- | Start a sig-submission server
  , aSigSubmissionServer :: ServerApp addr m b

    -- | Start a keep-alive client.
  , aKeepAliveClient     :: ClientApp addr m a

    -- | Start a keep-alive server.
  , aKeepAliveServer     :: ServerApp addr m b

    -- | Start a peer-sharing client.
  , aPeerSharingClient   :: ClientApp addr m a

    -- | Start a peer-sharing server.
  , aPeerSharingServer   :: ServerApp addr m b
  }

ntnApps
  :: forall m addr .
    ( Alternative (STM m)
    , MonadAsync m
    , MonadDelay m
    , MonadFork m
    , MonadMask m
    , MonadMVar m
    , MonadThrow (STM m)
    , MonadTimer m
    , Ord addr
    , Show addr
    , Hashable addr
    )
 => NodeKernel addr m
 -> Codecs addr m
 -> LimitsAndTimeouts addr
 -> TxDecisionPolicy
 -> Apps addr m () ()
ntnApps
    NodeKernel {
      fetchClientRegistry
    , peerSharingRegistry
    , peerSharingAPI
    , mempool
    , sigChannelVar
    , sigMempoolSem
    , sigSharedTxStateVar
    }
    Codecs {
      sigSubmissionCodec
    , keepAliveCodec
    , peerSharingCodec
    }
    LimitsAndTimeouts {
      sigSubmissionSizeLimits
    , sigSubmissionTimeLimits
    , keepAliveSizeLimits
    , keepAliveTimeLimits
    , peerSharingTimeLimits
    , peerSharingSizeLimits
    }
    sigDecisionPolicy
    =
    Apps {
      aSigSubmissionClient
    , aSigSubmissionServer
    , aKeepAliveClient
    , aKeepAliveServer
    , aPeerSharingClient
    , aPeerSharingServer
    }
  where
    sigSize :: Sig -> SizeInBytes
    sigSize _ = 0 -- TODO

    mempoolReader = Mempool.getReader sigId sigSize mempool
    mempoolWriter = Mempool.getWriter sigId sigValid mempool
      where
        -- Note: invalid signatures are just omitted from the mempool. For DMQ
        -- we need to validate signatures when we received them, and shutdown
        -- connection if we receive one, rather than validate them in the
        -- mempool.
        sigValid :: Sig -> Bool
        sigValid _ = True

    aSigSubmissionClient
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addr m
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aSigSubmissionClient version
                         ExpandedInitiatorContext {
                           eicControlMessage = controlMessage
                         } channel =
      runPeerWithLimits
        nullTracer
        sigSubmissionCodec
        sigSubmissionSizeLimits
        sigSubmissionTimeLimits
        channel
        $ txSubmissionClientPeer
        $ txSubmissionOutbound
            nullTracer
            _MAX_SIGS_TO_ACK
            mempoolReader
            version
            controlMessage


    aSigSubmissionServer
      :: NodeToNodeVersion
      -> ResponderContext addr
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aSigSubmissionServer _version ResponderContext { rcConnectionId = connId } channel =
        SigSubmission.withPeer
          nullTracer
          sigChannelVar
          sigMempoolSem
          sigDecisionPolicy
          sigSharedTxStateVar
          mempoolReader
          mempoolWriter
          sigSize
          (remoteAddress connId)
          $ \(peerSigAPI :: PeerTxAPI m SigId Sig) ->
            runPipelinedPeerWithLimits
              nullTracer
              sigSubmissionCodec
              sigSubmissionSizeLimits
              sigSubmissionTimeLimits
              channel
              $ txSubmissionServerPeerPipelined
              $ txSubmissionInboundV2
                  nullTracer
                  _SIG_SUBMISSION_INIT_DELAY
                  mempoolWriter
                  peerSigAPI


    aKeepAliveClient
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addr m
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aKeepAliveClient _version
                     ExpandedInitiatorContext {
                       eicConnectionId   = them
                     , eicControlMessage = controlMessageSTM
                     }
                     channel = do
      labelThisThread "KeepAliveClient"
      let kacApp dqCtx =
            runPeerWithLimits
              nullTracer
              keepAliveCodec
              keepAliveSizeLimits
              keepAliveTimeLimits
              channel
              $ keepAliveClientPeer
              $ keepAliveClient nullTracer
                                (mkStdGen 0)
                                controlMessageSTM
                                them
                                dqCtx
                                (KeepAliveInterval 10)

      ((), trailing) <- bracketKeepAliveClient fetchClientRegistry them kacApp
      return ((), trailing)

    aKeepAliveServer
      :: NodeToNodeVersion
      -> ResponderContext ntnAddr
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aKeepAliveServer _version
                     ResponderContext {
                       rcConnectionId = _them
                     }
                     channel = do
      labelThisThread "KeepAliveServer"
      runPeerWithLimits
        nullTracer
        keepAliveCodec
        keepAliveSizeLimits
        keepAliveTimeLimits
        channel
        $ keepAliveServerPeer
          keepAliveServer

    aPeerSharingClient
      :: NodeToNodeVersion
      -> ExpandedInitiatorContext addr m
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aPeerSharingClient _version
                       ExpandedInitiatorContext {
                         eicConnectionId   = them
                       , eicControlMessage = controlMessageSTM
                       }
                       channel = do
      labelThisThread "PeerSharingClient"
      bracketPeerSharingClient peerSharingRegistry (remoteAddress them)
        $ \controller -> do
          psClient <- peerSharingClient controlMessageSTM controller
          ((), trailing) <- runPeerWithLimits
            nullTracer
            peerSharingCodec
            peerSharingSizeLimits
            peerSharingTimeLimits
            channel
            (peerSharingClientPeer psClient)
          return ((), trailing)

    aPeerSharingServer
      :: NodeToNodeVersion
      -> ResponderContext addr
      -> Channel m BL.ByteString
      -> m ((), Maybe BL.ByteString)
    aPeerSharingServer _version
                       ResponderContext {
                         rcConnectionId = _them
                       }
                       channel = do
      labelThisThread "PeerSharingServer"
      runPeerWithLimits
        nullTracer
        peerSharingCodec
        peerSharingSizeLimits
        peerSharingTimeLimits
        channel
        $ peerSharingServerPeer
        $ peerSharingServer peerSharingAPI


data Protocols appType initiatorCtx responderCtx bytes m a b =
  Protocols {
    sigSubmissionProtocol :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b

    -- | keep-alive mini-protocol
    --
  , keepAliveProtocol    :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b

    -- | peer sharing mini-protocol
    --
  , peerSharingProtocol  :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b
  }

sigSubmissionMiniProtocolNum :: Mx.MiniProtocolNum
sigSubmissionMiniProtocolNum = Mx.MiniProtocolNum 11

nodeToNodeProtocols
  :: LimitsAndTimeouts addr
  -> Protocols appType initiatorCtx responderCtx bytes m a b
  -> NodeToNodeVersion
  -- ^ negotiated version number
  -> NodeToNodeVersionData
  -- ^ negotiated version data
  -> OuroborosBundle appType initiatorCtx responderCtx bytes m a b
nodeToNodeProtocols LimitsAndTimeouts {
                      sigSubmissionLimits
                    , keepAliveLimits
                    , peerSharingLimits
                    }
                    Protocols {
                      sigSubmissionProtocol
                    , keepAliveProtocol
                    , peerSharingProtocol
                    }
                    _version
                    NodeToNodeVersionData {
                      peerSharing
                    }
                    =
    TemperatureBundle
      -- Hot protocols
      (WithHot [
        MiniProtocol {
          miniProtocolNum    = sigSubmissionMiniProtocolNum
        , miniProtocolStart  = StartOnDemandAny
        , miniProtocolLimits = sigSubmissionLimits
        , miniProtocolRun    = sigSubmissionProtocol
        }
      ])

      -- Warm protocols
      (WithWarm [])

      -- Established protocols: 'keep-alive', 'peer-sharing'.
      (WithEstablished $
        MiniProtocol {
          -- TODO: we SHOULDN'T use cardano keep alive mini-protocol number
          miniProtocolNum    = keepAliveMiniProtocolNum
        , miniProtocolStart  = StartOnDemandAny
        , miniProtocolLimits = keepAliveLimits
        , miniProtocolRun    = keepAliveProtocol
        }
        : case peerSharing of
            PeerSharingEnabled ->
              [ MiniProtocol {
                  -- TODO: we SHOULDN'T use cardano peer sharing mini-protocol number
                  miniProtocolNum    = peerSharingMiniProtocolNum
                , miniProtocolStart  = StartOnDemand
                , miniProtocolLimits = peerSharingLimits
                , miniProtocolRun    = peerSharingProtocol
                }
              ]
            PeerSharingDisabled ->
              []
      )

initiatorProtocols
  :: LimitsAndTimeouts addr
  -> Apps addr m a b
  -> NodeToNodeVersion
  -> NodeToNodeVersionData
  -> OuroborosBundleWithExpandedCtx 'InitiatorMode addr BL.ByteString m a Void
initiatorProtocols limitsAndTimeouts
                   Apps {
                     aSigSubmissionClient
                   , aKeepAliveClient
                   , aPeerSharingClient
                   }
                   version =
  nodeToNodeProtocols
    limitsAndTimeouts
    (Protocols {
      sigSubmissionProtocol =
        InitiatorProtocolOnly (MiniProtocolCb (aSigSubmissionClient version))
    , keepAliveProtocol =
        InitiatorProtocolOnly (MiniProtocolCb (aKeepAliveClient version))
    , peerSharingProtocol =
        InitiatorProtocolOnly (MiniProtocolCb (aPeerSharingClient version))
    })
    version

initiatorAndResponderProtocols
  :: LimitsAndTimeouts addr
  -> Apps addr m a b
  -> NodeToNodeVersion
  -> NodeToNodeVersionData
  -> OuroborosBundleWithExpandedCtx 'InitiatorResponderMode addr BL.ByteString m a b
initiatorAndResponderProtocols limitsAndTimeouts
                               Apps {
                                 aSigSubmissionClient
                               , aSigSubmissionServer
                               , aKeepAliveClient
                               , aKeepAliveServer
                               , aPeerSharingClient
                               , aPeerSharingServer
                               }
                               version =
  nodeToNodeProtocols
    limitsAndTimeouts
    (Protocols {
      sigSubmissionProtocol =
        InitiatorAndResponderProtocol
           (MiniProtocolCb (aSigSubmissionClient version))
           (MiniProtocolCb (aSigSubmissionServer version))
    , keepAliveProtocol =
        InitiatorAndResponderProtocol
           (MiniProtocolCb (aKeepAliveClient version))
           (MiniProtocolCb (aKeepAliveServer version))
    , peerSharingProtocol =
        InitiatorAndResponderProtocol
           (MiniProtocolCb (aPeerSharingClient version))
           (MiniProtocolCb (aPeerSharingServer version))
    })
    version

data Codecs addr m =
  Codecs {
    sigSubmissionCodec :: Codec SigSubmission
                            CBOR.DeserialiseFailure m BL.ByteString
  , keepAliveCodec     :: Codec KeepAlive
                            CBOR.DeserialiseFailure m BL.ByteString
  , peerSharingCodec   :: Codec (Protocol.PeerSharing addr)
                            CBOR.DeserialiseFailure m BL.ByteString
  }

dmqCodecs :: MonadST m
          => (addr -> CBOR.Encoding)
          -> (forall s. CBOR.Decoder s addr)
          -> Codecs addr m
dmqCodecs encodeAddr decodeAddr =
  Codecs {
    sigSubmissionCodec = codecSigSubmission
  , keepAliveCodec     = codecKeepAlive_v2
  , peerSharingCodec   = codecPeerSharing encodeAddr decodeAddr
  }

data LimitsAndTimeouts addr =
  LimitsAndTimeouts {
    -- sig-submission
    sigSubmissionLimits
      :: MiniProtocolLimits
  , sigSubmissionSizeLimits
      :: ProtocolSizeLimits SigSubmission BL.ByteString
  , sigSubmissionTimeLimits
      :: ProtocolTimeLimits SigSubmission

    -- keep-alive
  , keepAliveLimits
      :: MiniProtocolLimits
  , keepAliveSizeLimits
      :: ProtocolSizeLimits KeepAlive BL.ByteString
  , keepAliveTimeLimits
      :: ProtocolTimeLimits KeepAlive

    -- peer sharing
  , peerSharingLimits
      :: MiniProtocolLimits
  , peerSharingTimeLimits
      :: ProtocolTimeLimits (Protocol.PeerSharing addr)
  , peerSharingSizeLimits
      :: ProtocolSizeLimits (Protocol.PeerSharing addr) BL.ByteString
  }

dmqLimitsAndTimeouts :: LimitsAndTimeouts addr
dmqLimitsAndTimeouts =
    LimitsAndTimeouts {
      sigSubmissionLimits =
        MiniProtocolLimits {
          -- TODO
          maximumIngressQueue = maxBound
        }
    , sigSubmissionTimeLimits = timeLimitsSigSubmission
    , sigSubmissionSizeLimits = byteLimitsSigSubmission size

    , keepAliveLimits     =
        MiniProtocolLimits {
          -- One small outstanding message.
          maximumIngressQueue = addSafetyMargin 1280
        }

    , keepAliveTimeLimits = timeLimitsKeepAlive
    , keepAliveSizeLimits = byteLimitsKeepAlive size

    , peerSharingLimits   =
        MiniProtocolLimits {
          -- This protocol does not need to be pipelined and a peer can only ask
          -- for a maximum of 255 peers each time. Hence a reply can have up to
          -- 255 IP (IPv4 or IPv6) addresses so 255 * 16 = 4080. TCP has an initial
          -- window size of 4 and a TCP segment is 1440, which gives us 4 * 1440 =
          -- 5760 bytes to fit into a single RTT. So setting the maximum ingress
          -- queue to be a single RTT should be enough to cover for CBOR overhead.
          maximumIngressQueue = 4 * 1440
        }
    , peerSharingTimeLimits = timeLimitsPeerSharing
    , peerSharingSizeLimits = byteLimitsPeerSharing size
    }
  where
    size :: BL.ByteString -> Word
    size = fromIntegral . BL.length


type HandshakeTr ntnAddr = Mx.WithBearer (ConnectionId ntnAddr) (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term))

ntnHandshakeArguments
  :: MonadST m
  => Tracer m (HandshakeTr ntnAddr)
  -> HandshakeArguments
      (ConnectionId ntnAddr)
      NodeToNodeVersion
      NodeToNodeVersionData
      m
ntnHandshakeArguments tracer =
  HandshakeArguments {
    haHandshakeTracer  = tracer
  , haBearerTracer     = nullTracer -- TODO
  , haHandshakeCodec   = codecHandshake nodeToNodeVersionCodec
  , haVersionDataCodec = cborTermVersionDataCodec nodeToNodeCodecCBORTerm
  , haAcceptVersion    = acceptableVersion
  , haQueryVersion     = queryVersion
  , haTimeLimits       = timeLimitsHandshake
  }

stdVersionDataNTN :: NetworkMagic
                  -> DiffusionMode
                  -> PeerSharing
                  -> NodeToNodeVersionData
stdVersionDataNTN networkMagic diffusionMode peerSharing =
  NodeToNodeVersionData
    { networkMagic
    , diffusionMode
    , peerSharing
    , query = False
    }

-- TODO: choose wisely, is a protocol parameter.
_MAX_SIGS_TO_ACK :: NumTxIdsToAck
_MAX_SIGS_TO_ACK = 20

_SIG_SUBMISSION_INIT_DELAY :: TxSubmissionInitDelay
_SIG_SUBMISSION_INIT_DELAY = NoTxSubmissionInitDelay
