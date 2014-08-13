{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies #-}
-- | This module provides a simple ChatCore protocol that just exposes Chat
-- Core's internal event system through a simple JSON interface.
-- It's not supported by any client, but it provides a good baseline as well as
-- a good way to test Chat Core's event system.
module ChatCore.Protocol.JSON
    ( JSONCoreType
    , jsonCoreType
    , JSONConnection
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Conduit
import Data.Maybe
import qualified Data.Conduit.List as CL
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Typeable
import Network
import System.IO

import ChatCore.Events
import ChatCore.Protocol
import ChatCore.Types

jsonCoreType :: JSONCoreType
jsonCoreType = JSONCoreType { jsonCorePort = PortNumber 1337 }

-- | Data structure for the JSON core type.
data JSONCoreType = JSONCoreType
    { jsonCorePort :: PortID -- The port to listen on.
    } deriving (Typeable)

instance CoreType JSONCoreType JSONConnection where
    coreTypeName _ = "JSON Serialization"
    coreTypeDesc _ = "A protocol that simply exposes Chat Core's internal event system via JSON serialization."
    connectionListener coreType = do
        liftIO $ putStrLn "JSON core type is listening."
        -- Wait for a connection.
        sock <- liftIO $ listenOn (jsonCorePort coreType)
        -- Accept connections.
        forever $ acceptConnection sock

-- | Accepts a new connection on the given socket.
acceptConnection :: Socket -> ConnectionListener ()
acceptConnection sock = do
    liftIO $ putStrLn "JSON connection listener is awaiting a new connection."
    -- Accept the sock.
    (handle, host, port) <- liftIO $ accept sock
    -- Create the connection object.
    let conn = JSONConnection {
          jcHandle = handle
        , jcRemoteHost = host
        , jcPortNumber = port
        , jcSocket = sock
        }
    -- Give an IO action that just returns our connection object.
    -- If we wanted to do any processing, we would do so inside this IO
    -- action.
    newConnection $ return conn


-- | Data structure representing a JSON core connection.
data JSONConnection = JSONConnection
    { jcHandle     :: Handle       -- | IO handle for reading and writing.
    , jcRemoteHost :: HostName     -- | The client's hostname.
    , jcPortNumber :: PortNumber   -- | The port number.
    , jcSocket     :: Socket       -- | The socket to receive data from.
    } deriving (Typeable)

instance CoreProtocol JSONConnection where
    eventListener conn =
        src $= (CL.map $ decodeStrict)
            $= (CL.filter $ isJust)
            $= (CL.map $ fromJust)
            $= (CL.mapM $ \c -> (putStrLn $ show c) >> return c)
        --src $= (CL.mapM_ $ B.putStrLn)
      where
        src = (yield =<< (lift $ B.hGetLine $ jcHandle conn)) >> src

    sendEvent conn msg = do
        BL.hPutStr (jcHandle conn) $ encode msg


-- {{{ JSON

-- {{{ JSON Parsing

instance FromJSON ClientCommand where
    parseJSON (Object obj) = do
        cmdType <- obj .: "command"
        case cmdType :: T.Text of
             "sendmsg" -> SendMessage
                <$> obj .:   "network"
                <*> obj .:   "dest"
                <*> obj .:   "message"
             "joinchan" -> JoinChannel
                <$> obj .:   "network"
                <*> obj .:   "channel"
             "partchan" -> PartChannel
                <$> obj .:   "network"
                <*> obj .:   "channel"
                <*> obj .:?  "message"

-- }}}

-- {{{ JSON Serializing

instance ToJSON CoreEvent where
    toJSON evt@(ReceivedMessage {}) = object
        [ "event"       .= ("recvmsg" :: T.Text)
        , "source"      .= recvMsgSource evt
        , "sender"      .= recvMsgSender evt
        , "message"     .= recvMsgContent evt
        ]

-- }}}

-- }}}

