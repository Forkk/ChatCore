module ChatCore.Protocol.Quassel
    ( QuasselCoreType
    , quasselCoreType
    , QuasselConn
    ) where

import Control.Applicative
import Control.Error
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
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


quasselCoreType :: QuasselCoreType
quasselCoreType = QuasselCoreType { quasselCorePort = PortNumber 4242 }

-- | Data structure for the Quassel core type.
data QuasselCoreType = QuasselCoreType
    { quasselCorePort :: PortID -- The port to listen on.
    } deriving (Typeable)

instance CoreType QuasselCoreType QuasselConn where
    coreTypeName _ = "Quassel Emulation"
    coreTypeDesc _ = "A core protocol type that emulates a Quassel Core."
    connectionListener coreType = do
        liftIO $ putStrLn "Quassel core type is listening."
        -- Wait for a connection.
        sock <- liftIO $ listenOn (quasselCorePort coreType)
        -- Accept connections.
        forever $ acceptConnection sock

-- | Accepts a new connection on the given socket.
acceptConnection :: Socket -> ConnectionListener ()
acceptConnection sock = do
    (handle, host, port) <- liftIO $ accept sock
    liftIO $ putStrLn "Got Quassel connection."
    let printErr :: SomeException -> IO (Maybe QuasselConn)
        printErr e = print e >> return Nothing
    connM <- liftIO $ catch (Just <$> initConn host port handle) printErr
    if isJust connM
       then newConnection $ fromJust connM
       else return ()


-- | Data structure representing a Quassel Core connection.
data QuasselConn = QuasselConn
    { qcHandle      :: Handle       -- | IO handle for reading and writing.
    , qcSource      :: NetworkSrc   -- | Resumable source to read from.
    , qcRemoteHost  :: HostName     -- | The client's hostname.
    , qcPortNumber  :: PortNumber   -- | The port number.
    } deriving (Typeable)

-- | Data structure with client information.
data ClientInfo = ClientInfo
    { ciDate    :: UTCTime
    , ciUseSSL  :: Bool
    , ciVersion :: T.Text
    , ciCmpress :: Bool
    }

instance CoreProtocol QuasselConn where
    eventListener conn = do
        return ()

    sendEvent conn msg = do
        return ()


initConn :: HostName -> PortNumber -> Handle -> IO QuasselConn
initConn host port handle = (flip onException) (hClose handle) $ do
    (src, features) <- liftIO (sourceHandle handle $$+ sinkConnFeatures)
    -- Select a protocol. We'll use the first one we support.
    -- TODO: Support other protocols.
    let protocol = headNote "No supported protocol." $
                       filter ((==DataStreamProtocol) . pclType) $
                           cfProtocols features
    -- Tell the client which protocol we've chosen.
    BL.hPut handle $ runPut $ putProtocol protocol
    -- Do the secret handshake.
    doHandshake host port handle src
    -- Create a connection object.
    let conn = QuasselConn {
          qcHandle = handle
        , qcSource = src
        , qcRemoteHost = host
        , qcPortNumber = port
        }
    -- We're done.
    return conn

-- | Handles handshake messages from the client until it is time to switch to
-- the signal proxy system.
doHandshake :: HostName -> PortNumber -> Handle -> NetworkSrc -> IO QuasselConn
doHandshake host port handle netSrc = do
    (netSrc', msg) <- readHandshakeMsg netSrc
    handleHandshakeMsg handle msg
    doHandshake host port handle netSrc'

handleHandshakeMsg :: Handle -> HandshakeMsg -> IO ()
handleHandshakeMsg handle (RegisterClientMsg vsn bdate) = do
    -- TODO
    sendHandshakeMsg handle $ ClientRegisteredMsg 0x00000000
handleHandshakeMsg handle (LoginMsg user passwd) = do
    putStrLn ("Login from " ++ T.unpack user)
    sendHandshakeMsg handle $ LoginSuccessMsg
handleHandshakeMsg handle msg = do
    putStrLn ("Unhandled handshake message: " ++ show msg)


-- {{{ Read / Write Messages

readHandshakeMsg :: NetworkSrc -> IO (NetworkSrc, HandshakeMsg)
readHandshakeMsg netSrc = do
    (netSrc', msgData) <- readMsgBytes netSrc
    let msg = runGet getHandshakeMsg $ BL.fromChunks [msgData]
    return (netSrc', msg)

readMsgBytes :: NetworkSrc -> IO (NetworkSrc, B.ByteString)
readMsgBytes netSrc = netSrc $$++ sinkGet getMessageBytes


sendHandshakeMsg :: Handle -> HandshakeMsg -> IO ()
sendHandshakeMsg handle msg = do
    let msgData = runPut $ putHandshakeMsg msg
    -- Write the message size.
    BL.hPut handle $ runPut $ putWord32be $ fromIntegral $ BL.length msgData
    -- Write the message.
    BL.hPut handle $ msgData

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

