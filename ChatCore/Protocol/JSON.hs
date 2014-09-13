-- | This module provides a simple ChatCore protocol that just exposes Chat
-- Core's internal event system through a simple JSON interface.
-- It's not supported by any client, but it provides a good baseline as well as
-- a good way to test Chat Core's event system.
module ChatCore.Protocol.JSON
    ( jsonConnListener
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


-- | Creates a JSON connection listener listening on the given port.
jsonConnListener :: PortID -> ConnListener
jsonConnListener port = ConnListener
    { listenerFunc = listenFunc port
    , clName = "JSON Core Protocol"
    , clId = "json"
    }


-- | Client listener function for Chat Core's JSON protocol.
listenFunc :: PortID -> Source IO PendingConn
listenFunc port = do
    -- Wait for a connection.
    sock <- liftIO $ listenOn port
    -- Accept connections.
    forever $ acceptConnection sock

-- | Accepts a new connection on the given socket.
acceptConnection :: Socket -> Source IO PendingConn
acceptConnection sock = do
    liftIO $ putStrLn "JSON connection listener is awaiting a connection."
    -- Accept the sock.
    (handle, host, port) <- liftIO $ accept sock
    -- Create a pending connection from the handle.
    yield $ pendingConn handle host port


-- | Handles authentication an initialization for the JSON protocol.
pendingConn :: Handle -> HostName -> PortNumber -> PendingConn
pendingConn handle host port coreCtl = do
    liftIO $ putStrLn "JSON connection pending..."
    -- TODO: Implement authentication for the JSON protocol.
    return ("Forkk", connection handle host port)


-- {{{ Connection Function

-- | The `RemoteClient` function.
connection :: Handle -> HostName -> PortNumber -> RemoteClient ()
connection handle host port = do
    liftIO $ putStrLn "JSON connection started."
    val <- coreEvtOr $ B.hGetLine handle
    case val of
         Left ce -> handleCoreEvt handle ce
         Right msgData ->
             maybe (return ()) handleClientMsg =<< parseClientMsgM msgData
    connection handle host port
  where
    -- Handle core events or client commands.
    evtHandler (Left evt) = handleCoreEvt handle evt
    evtHandler (Right msg) = handleClientMsg msg
    -- Parses the given message from the client.
    parseClientMsgM msgData = do
        -- Log the error if the parsing failed. Otherwise, convert the
        -- Either to a Maybe and pass it on.
        logParseError $ eitherDecodeStrict msgData
    -- If the given argument is a parse error message, log it.
    logParseError (Right cmd) = do
        return $ Just cmd
    logParseError (Left err) = do
        liftIO $ putStrLn ("Error parsing JSON message: " ++ err)
        return Nothing

-- }}}


-- | Handles a message from the client.
handleClientMsg :: ClientCommand -> RemoteClient ()
handleClientMsg = receivedClientCmd

-- | Handles a core event.
handleCoreEvt :: Handle -> CoreEvent -> RemoteClient ()
handleCoreEvt handle evt = do
    liftIO $ TL.hPutStrLn handle $ TL.decodeUtf8 $ encode evt


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

