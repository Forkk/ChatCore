module ChatCore.Protocol.Quassel
    ( quasselConnListener
    ) where

import Control.Applicative
import Control.Error
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State as St
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Typeable
import Data.Int
import Data.Word
import Network
import System.IO
import Safe
import Language.Literals.Binary

import ChatCore.Events
import ChatCore.Protocol
import ChatCore.Protocol.Quassel.Message
import ChatCore.Protocol.Quassel.Types
import ChatCore.Protocol.Quassel.QVariant
import ChatCore.Types

-- | Creates a connection listener listening on the given port.
quasselConnListener :: PortID -> ConnListener
quasselConnListener port = ConnListener
    { listenerFunc = listenFunc port
    , clName = "Quassel Core Protocol"
    , clId = "quassel"
    }


-- | Client listener function.
listenFunc :: PortID -> Source IO PendingConn
listenFunc port = do
    -- Wait for a connection.
    sock <- liftIO $ listenOn port
    -- Accept connections.
    forever $ acceptConnection sock

-- | Accepts a new connection on the given socket.
acceptConnection :: Socket -> Source IO PendingConn
acceptConnection sock = do
    liftIO $ putStrLn "Quassel connection listener is awaiting a connection."
    -- Accept the sock.
    (handle, host, port) <- liftIO $ accept sock
    -- Create a pending connection from the handle.
    yield $ pendingConn handle


-- | Handles authentication an initialization.
pendingConn :: Handle -> PendingConn
pendingConn handle coreCtl = do
    liftIO $ putStrLn "Quassel connection pending. Checking features."
    -- TODO: Support other protocols.
    -- Select a protocol. We'll use the first one we support.
    (ns, features) <- liftIO (sourceHandle handle $$+ sinkConnFeatures)
    let protocol = headNote "No supported protocol." $
                       filter ((==DataStreamProtocol) . pclType) $
                           cfProtocols features
    -- Tell the client which protocol we've chosen.
    BL.hPut handle $ runPut $ putProtocol protocol
    liftIO $ putStrLn "Quassel connection handshake."
    (info, ns') <- doHandshake handle ns
    liftIO $ putStrLn "Quassel connection initiated."
    return (hiUser info, connection handle ns)


-- {{{ Handshake Phase

-- | Data structure containing information gathered during the handshake
-- process.
data HandshakeInfo = HandshakeInfo
    { hiUser :: UserId
    , hiPass :: UserPassword
    }

-- | Handles handshake messages from the client until it is time to switch to
-- the signal proxy system.
doHandshake :: Handle -> NetworkSrc -> IO (HandshakeInfo, NetworkSrc)
doHandshake handle netSrc = (flip runStateT $ netSrc) $ do
    -- The client should send this message first. If not, crash the connection.
    (RegisterClientMsg vsn bdate) <- readHandshakeMsg
    -- Send a client registered message back.
    liftIO $ sendHandshakeMsg handle $ ClientRegisteredMsg 0x00000000 -- No extra features yet.
    -- Wait for a login message.
    (LoginMsg user pass) <- readHandshakeMsg
    -- Return the handshake info.
    return $ HandshakeInfo user pass

-- }}}

-- {{{ Read / Write Messages

readMsgBytes :: NetworkSrc -> IO (NetworkSrc, B.ByteString)
readMsgBytes netSrc = netSrc $$++ sinkGet getMessageBytes

readHandshakeMsg :: StateT NetworkSrc IO HandshakeMsg
readHandshakeMsg = do
    netSrc <- St.get
    (netSrc', msgData) <- liftIO $ readMsgBytes netSrc
    St.put netSrc'
    return $ runGet getHandshakeMsg $ BL.fromChunks [msgData]

sendHandshakeMsg :: Handle -> HandshakeMsg -> IO ()
sendHandshakeMsg handle msg = do
    let msgData = runPut $ putHandshakeMsg msg
    -- Write the message size.
    BL.hPut handle $ runPut $ putWord32be $ fromIntegral $ BL.length msgData
    -- Write the message.
    BL.hPut handle $ msgData

readSigProxyMsg :: StateT NetworkSrc IO SignalProxyMsg
readSigProxyMsg = do
    netSrc <- St.get
    (netSrc', msgData) <- liftIO $ readMsgBytes netSrc
    St.put netSrc'
    return $ runGet getSigProxyMsg $ BL.fromChunks [msgData]

sendSigProxyMsg :: Handle -> SignalProxyMsg -> IO ()
sendSigProxyMsg handle msg = do
    return ()
    {-
    let msgData = runPut $ putSigProxyMsg msg
    -- Write the message size.
    BL.hPut handle $ runPut $ putWord32be $ fromIntegral $ BL.length msgData
    -- Write the message.
    BL.hPut handle $ msgData
    -}

-- }}}

-- {{{ Sink Get

-- | Runs the given Get monad incrementally as a sink.
sinkGet :: (Monad m, MonadIO m) => Get a -> Sink B.ByteString m a
sinkGet getF = sinkGet' $ runGetIncremental getF
  where
    sinkGet' (Partial readNext) = do
        -- Await further input from the handle.
        input <- await
        sinkGet' $ readNext input
    sinkGet' (Done unread _ result) = do
        -- Put the unused input back upstream and finish reading.
        leftover unread
        return result
    sinkGet' (Fail unread _ msg) = do
        leftover unread
        fail ("Failed reading data: " ++ msg)

sinkGetQV :: (Monad m, MonadIO m, QVariantVal a) => Sink B.ByteString m a
sinkGetQV = sinkGet getQV

-- }}}

-- {{{ Connection Handshake

-- | Sink which reads the connection features sent by the client.
sinkConnFeatures :: Sink B.ByteString IO ConnFeatures
sinkConnFeatures = sinkGet getConnFeatures

-- | Quassel's "magic number" sent at the beginning of a connection.
magicNumber :: Word32
magicNumber = 0x42B33F00

-- | True if the given word matches the magic number.
magicNumberMatches :: Word32 -> Bool
magicNumberMatches val = val .&. magicNumber == magicNumber


sslFlag :: Word32
sslFlag = [b| 00000000 00000000 00000000 00000001 |]

compressionFlag :: Word32
compressionFlag = [b| 00000000 00000000 00000000 00000010 |]


-- | Data structure for holding information the client sends about its
-- supported connection settings.
-- This includes the client's supported protocol list and the requested global
-- connection settings such as SSL and compression.
data ConnFeatures = ConnFeatures
    { cfSSL         :: Bool
    , cfCompression :: Bool
    , cfProtocols   :: [ProtocolListEntry]
    } deriving (Show)

-- | Enum representing an entry in the handshake's protocol list.
data ProtocolListEntry
    = QuasselProtocol
        { pclType   :: ProtocolType
        , pclFlags  :: Word16
        , plcFeats  :: Word16
        }
    | UnknownProtocol Word16 Word16
    deriving (Show, Eq)


-- | A `Get` action which reads `ConnFeatures` sent by the client.
getConnFeatures :: Get ConnFeatures
getConnFeatures = do
    -- Read the first Word32 to get the magic number and global flags.
    initWord <- getWord32be
    -- Fail if the magic number is invalid.
    if magicNumberMatches initWord
       then return () else fail ("Invalid magic number: " ++ show initWord)
    -- Check the flags.
    let useSSL = initWord .&. sslFlag /= 0
        useCompression = initWord .&. compressionFlag /= 0
    -- Read the protocol list.
    pcols <- getProtocols
    return $ ConnFeatures
        { cfSSL = useSSL
        , cfCompression = useCompression
        , cfProtocols = pcols
        }

-- | Reads the protocol list.
getProtocols :: Get [ProtocolListEntry]
getProtocols = do
    pcol <- getWord32be
    -- Check the lower 8 bits for the protocol ID. Get the protocol options
    -- from bits 9-23. Connection features are in the last byte. The last bit
    -- indicates the end of the list.
    let pcolId  = (pcol .&. [b| 00000000 00000000 00000000 11111111 |])
        opts    = (pcol .&. [b| 00000000 01111111 11111111 00000000 |]) `shift` (-8)
        feats   = (pcol .&. [b| 01111111 10000000 00000000 00000000 |]) `shift` (-16 -7)
        listEnd = (pcol .&. [b| 10000000 00000000 00000000 00000000 |])/=0
        -- Construct a protocol object based on the above info.
        pcolTypeM= case pcolId of
            -- Legacy. This ignores all options and features.
            0x01 -> Just LegacyProtocol
            0x02 -> Just DataStreamProtocol
            _    -> Nothing
        pcolInfo = case pcolTypeM of
            Just p ->
                QuasselProtocol p (fromIntegral opts) (fromIntegral feats)
            Nothing ->
                UnknownProtocol (fromIntegral opts) (fromIntegral feats)
    -- If we've hit the end of the protocol list, return the final protocol.
    -- Otherwise, prepend the protocol we just read to the rest of the protocol
    -- list.
    if listEnd
       then return [pcolInfo]
       else (pcolInfo:) <$> getProtocols

-- | Writes the given protocol object to binary.
putProtocol :: ProtocolListEntry -> Put
putProtocol (QuasselProtocol DataStreamProtocol opts flags) = do
    putWord32be 0x00000002
putProtocol _ = do
    fail "Unimplemented protocol."

-- }}}

-- {{{ Connection Function

-- | The `RemoteClient` function.
connection :: Handle -> NetworkSrc -> RemoteClient ()
connection handle ns = do
    -- TODO: At the beginning of the connection, send the session state and
    -- switch to signal proxy mode.
    liftIO $ putStrLn "Quassel connection started."
    val <- coreEvtOr $ runStateT readSigProxyMsg ns
    case val of
         Left ce -> do
             handleCoreEvt handle ce
             connection handle ns
         Right (msg, ns') -> do
             handleClientMsg msg
             connection handle ns'

-- }}}

-- | Handles a message from the client.
handleClientMsg :: SignalProxyMsg -> RemoteClient ()
handleClientMsg msg = do
    liftIO $ putStrLn "Got quassel message."
    liftIO $ print msg
    return ()

-- | Handles a core event.
handleCoreEvt :: Handle -> CoreEvent -> RemoteClient ()
handleCoreEvt handle evt = do
    return ()
    -- liftIO $ TL.hPutStrLn handle $ TL.decodeUtf8 $ encode evt

