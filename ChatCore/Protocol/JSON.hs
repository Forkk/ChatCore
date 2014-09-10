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
import Control.Error
import Control.Monad
import Control.Monad.Trans
import Data.Conduit
import Data.Either
import Data.Maybe
import qualified Data.Conduit.List as CL
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
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
        }
    -- Give an IO action that just returns our connection object.
    -- If we wanted to do any processing, we would do so inside this IO
    -- action.
    newConnection conn


-- | Data structure representing a JSON core connection.
data JSONConnection = JSONConnection
    { jcHandle     :: Handle       -- | IO handle for reading and writing.
    , jcRemoteHost :: HostName     -- | The client's hostname.
    , jcPortNumber :: PortNumber   -- | The port number.
    } deriving (Typeable)

instance CoreProtocol JSONConnection where
    eventListener conn = do
        src $= (CL.map $ eitherDecodeStrict)
            -- Log the error if the parsing failed. Otherwise, convert the
            -- Either to a Maybe and pass it on.
            $= (CL.mapMaybeM $ logParseError)
      where
        src = (yield =<< (lift $ B.hGetLine $ jcHandle conn)) >> src
        -- If the given argument is a parse error message, log it.
        logParseError (Right cmd) = do
            return $ Just cmd
        logParseError (Left err) = do
            liftIO $ putStrLn ("Error parsing JSON message: " ++ err)
            return Nothing

    sendEvent conn msg = do
        let h = jcHandle conn
        TL.hPutStrLn h $ TL.decodeUtf8 $ encode msg

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
                <*> obj .:?  "msgtype" .!= MtPrivmsg
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
    toJSON (BufCoreEvent chatNet chatBuf evt) = object $
        [ "network"     .= chatNet
        , "buffer"      .= chatBuf
        ] ++ bufEvtPairs evt

-- }}}

-- }}}

