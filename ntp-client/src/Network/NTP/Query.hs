{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}

module Network.NTP.Query
where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (bracket)
import           System.IO.Error (tryIOError, userError, ioError)
import           Control.Monad (forever, forM, forM_, when)
import           Control.Tracer
import           Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (find)
import           Data.Maybe
import           Network.Socket ( AddrInfo
                                , AddrInfoFlag (AI_ADDRCONFIG, AI_PASSIVE)
                                , Family (AF_INET, AF_INET6)
                                , PortNumber
                                , Socket
                                , SockAddr (..)
                                , SocketOption (ReuseAddr)
                                , SocketType (Datagram)
                                , addrFamily
                                , addrFlags
                                , addrSocketType)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.ByteString (recvFrom, sendManyTo)
import           Network.NTP.Packet ( mkNtpPacket
                                    , ntpPacketSize
                                    , Microsecond
                                    , NtpOffset (..)
                                    , getCurrentTime
                                    , clockOffsetPure)
import           Network.NTP.Trace (NtpTrace (..), IPVersion (..))

data NtpClientSettings = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ List of servers addresses.
    , ntpResponseTimeout :: Microsecond
      -- ^ Timeout between sending NTP requests and response collection.
    , ntpPollDelay       :: Microsecond
      -- ^ How long to wait between two rounds of requests.
    , ntpReportPolicy    :: [NtpOffset] -> Maybe NtpOffset
    }

data NtpStatus =
      -- | The difference between NTP time and local system time
      NtpDrift NtpOffset
      -- | NTP client has send requests to the servers
    | NtpSyncPending
      -- | NTP is not available: the client has not received any respond within
      -- `ntpResponseTimeout` or NTP was not configured.
    | NtpSyncUnavailable deriving (Eq, Show)

-- | Wait for at least three replies and report the minimum of the reported offsets.
minimumOfThree :: [NtpOffset] -> Maybe NtpOffset
minimumOfThree l
    = if length l >= 3 then Just $ minimum l
         else Nothing

udpLocalAddresses :: IO [AddrInfo]
udpLocalAddresses = do
    let hints = Socket.defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Datagram }
        port = Socket.defaultPort
    --                 Hints        Host    Service
    Socket.getAddrInfo (Just hints) Nothing (Just $ show port)

resolveHost :: String -> IO [AddrInfo]
resolveHost host = Socket.getAddrInfo (Just hints) (Just host) Nothing
  where
    hints = Socket.defaultHints
            { addrSocketType = Datagram
            , addrFlags = [AI_ADDRCONFIG]  -- since we use @AF_INET@ family
            }

firstAddr :: String -> [AddrInfo] -> IO (Maybe AddrInfo, Maybe AddrInfo)
firstAddr name l = case (find isV4Addr l, find isV6Addr l) of
    (Nothing, Nothing) -> ioError $ userError $ "lookup host failed :" ++ name
    p -> return p
    where
        isV4Addr :: AddrInfo -> Bool
        isV4Addr addr = addrFamily addr == AF_INET

        isV6Addr :: AddrInfo -> Bool
        isV6Addr addr = addrFamily addr == AF_INET6

setNtpPort :: SockAddr ->  SockAddr
setNtpPort addr = case addr of
    (SockAddrInet  _ host)            -> SockAddrInet  ntpPort host
    (SockAddrInet6 _ flow host scope) -> SockAddrInet6 ntpPort flow host scope
    sockAddr                   -> sockAddr
  where
    ntpPort :: PortNumber
    ntpPort = 123

-- | Setup and run the NTP client.
-- In case of an IOError (for example when network interface goes down) cleanup and return.

oneshotClient  ::
       Tracer IO NtpTrace
    -> NtpClientSettings
    -> IO NtpStatus
oneshotClient tracer ntpSettings = do
    traceWith tracer NtpTraceClientStartQuery
    (v4Servers,   v6Servers)   <- lookupServers $ ntpServers ntpSettings
    (v4LocalAddr, v6LocalAddr) <- udpLocalAddresses >>= firstAddr "localhost"
-- TODO: bug here !!
-- this is a race-condition runProtocol can throw IO erorr !!
-- NtpTracePacketSentError Network.Socket.ByteString.sendManyTo: does not exist (Network is unreachable)
    (v4Replies, v6Replies) <- concurrently (runProtocol IPv4 v4LocalAddr v4Servers)
                                           (runProtocol IPv6 v6LocalAddr v6Servers)
    when (null v4Replies && null v6Replies) $ do
        traceWith tracer NtpTraceIPv4IPv6BothFailed
        ioError $ userError "IPv4 and IPv6 failed"
    status <- case (ntpReportPolicy ntpSettings) (v4Replies ++ v6Replies) of
        Nothing -> do
            traceWith tracer NtpTraceUpdateStatusQueryFailed
            return NtpSyncUnavailable
        Just offset -> do
            traceWith tracer $ NtpTraceUpdateStatusClockOffset $ getNtpOffset offset
            return $ NtpDrift offset
    return status
    where
        runProtocol :: IPVersion -> Maybe AddrInfo -> [AddrInfo] -> IO [NtpOffset]
        runProtocol _version _localAddr [] = return []
        runProtocol _version Nothing    _  = return []
        runProtocol version (Just addr) servers = do
             runNtpQueries tracer ntpSettings addr servers >>= \case
                Left err -> do
                    traceWith tracer $ NtpTraceRunProtocolError version err
                    return []
                Right [] -> do
                    traceWith tracer $ NtpTraceRunProtocolNoResult version
                    return []
                Right r@(_:_) -> do
                    traceWith tracer $ NtpTraceRunProtocolSuccess version
                    return r

runNtpQueries ::
       Tracer IO NtpTrace
    -> NtpClientSettings
    -> AddrInfo
    -> [AddrInfo]
    -> IO (Either IOError [NtpOffset])
runNtpQueries tracer netSettings localAddr destAddrs
    = tryIOError $ bracket acquire release action
  where
    acquire :: IO Socket
    acquire = do
        s <- Socket.socket (addrFamily localAddr) Datagram Socket.defaultProtocol
        traceWith tracer NtpTraceSocketOpen
        return s

    release :: Socket -> IO ()
    release s = do
        Socket.close s
        traceWith tracer NtpTraceSocketClosed

    action :: Socket -> IO [NtpOffset]
    action socket = do
        Socket.setSocketOption socket ReuseAddr 1
        inQueue <- atomically $ newTVar []
        _err <- withAsync (send socket  >> loopForever)  $ \sender ->
                withAsync timeout                        $ \delay ->
                withAsync (reader socket inQueue )       $ \revc ->
                    waitAnyCancel [sender, delay, revc]        
        atomically $ readTVar inQueue

    send :: Socket -> IO ()
    send sock = forM_ destAddrs $ \addr -> do
        p <- mkNtpPacket
        err <- tryIOError $ Socket.ByteString.sendManyTo sock
                          (LBS.toChunks $ encode p) (setNtpPort $ Socket.addrAddress addr)
        case err of
            Right _ -> traceWith tracer NtpTracePacketSent
            Left e  -> do
                traceWith tracer $ NtpTracePacketSentError e
                ioError e
        threadDelay 100_000

    loopForever = forever $ threadDelay maxBound

    timeout = do
        threadDelay $ (fromIntegral $ ntpResponseTimeout netSettings) + 100_000 * length destAddrs
        traceWith tracer NtpTraceClientWaitingForRepliesTimeout

    reader :: Socket -> TVar [NtpOffset] -> IO ()
    reader socket inQueue = forever $ do
        (bs, _) <- Socket.ByteString.recvFrom socket ntpPacketSize
        t <- getCurrentTime
        case decodeOrFail $ LBS.fromStrict bs of
            Left  (_, _, err) -> traceWith tracer $ NtpTraceSocketReaderDecodeError err
            Right (_, _, packet) -> do
            -- todo : filter bad packets, i.e. late packets and spoofed packets
                traceWith tracer NtpTraceReceiveLoopPacketReceived
                let offset = (clockOffsetPure packet t)
                atomically $ modifyTVar' inQueue ((:) offset)

lookupServers :: [String] -> IO ([AddrInfo], [AddrInfo])
lookupServers names = do
   dests <- forM names $ \server -> resolveHost server >>= firstAddr server
   return (mapMaybe fst dests, mapMaybe snd dests)
