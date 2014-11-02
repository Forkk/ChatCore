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
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network
import System.IO

import ChatCore.Events
import ChatCore.Types
import ChatCore.Util
import ChatCore.Protocol
import ChatCore.IRC


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
pendingConn handle host port _ = do
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
         Right msgData -> dropMaybeT $ do
             msg <- MaybeT $ parseClientMsgM msgData
             lift $ handleClientMsg msg
    connection handle host port
  where
    -- Parses the given message from the client.
    parseClientMsgM msgData =
        -- Log the error if the parsing failed. Otherwise, convert the
        -- Either to a Maybe and pass it on.
        logParseError $ decodeClientCmd msgData
    -- If the given argument is a parse error message, log it.
    logParseError (Right cmd) =
        return $ Just cmd
    logParseError (Left errMsg) = do
        liftIO $ putStrLn ("Error parsing JSON message: " ++ errMsg)
        return Nothing

-- }}}


-- | Handles a message from the client.
handleClientMsg :: ClientCommand -> RemoteClient ()
handleClientMsg = receivedClientCmd

-- | Handles a core event.
handleCoreEvt :: Handle -> CoreEvent -> RemoteClient ()
handleCoreEvt handle evt =
    liftIO $ TL.hPutStrLn handle $ TL.decodeUtf8 $ encodeCoreEvt evt


-- {{{ JSON

-- {{{ JSON Parsing

decodeClientCmd :: B.ByteString -> Either String ClientCommand
decodeClientCmd bs = parseEither clientCmdFromJSON =<< eitherDecodeStrict bs

clientCmdFromJSON :: Value -> Parser ClientCommand
clientCmdFromJSON (Object obj) = do
    cmdType <- obj .: "command"
    case cmdType :: T.Text of
            "send-msg" -> SendMessage
                <$> obj .:   "network"
                <*> obj .:   "dest"
                <*> obj .:   "message"
                <*> obj .:?  "msgtype" .!= MtPrivmsg
            "join-chan" -> JoinChannel
                <$> obj .:   "network"
                <*> obj .:   "channel"
            "part-chan" -> PartChannel
                <$> obj .:   "network"
                <*> obj .:   "channel"
                <*> obj .:?  "message"
            _ -> error "Unknown command type."
clientCmdFromJSON _ =
    error "Invalid client command message. Must be an object."

-- }}}

-- {{{ JSON Serializing

encodeCoreEvt :: CoreEvent -> BL.ByteString
encodeCoreEvt = encode . coreEvtToJSON

coreEvtToJSON :: CoreEvent -> Value
coreEvtToJSON (ChatBufferEvent chatNet chatBuf evt) = object $
    [ "network"     .= chatNet
    , "buffer"      .= chatBuf
    ] ++ bufEvtPairs evt
coreEvtToJSON (ChatNetworkEvent chatNet evt) = object $
    ("network" .= chatNet) : netEvtPairs evt

-- }}}

-- {{{ Event to JSON

ircSourceStr :: IRCSource -> T.Text
ircSourceStr (ServerSource serv) = serv
ircSourceStr (UserSource (IRCUser { _iuNick = nick })) = nick

bufEvtPairs :: BufferEvent -> [Pair]
bufEvtPairs (UserMessage sender content) =
    [ "event"       .= ("message" :: T.Text)
    , "sender"      .= ircSourceStr sender
    , "message"     .= content
    ]
bufEvtPairs (NoticeMessage sender content) =
    [ "event"       .= ("notice" :: T.Text)
    , "sender"      .= ircSourceStr sender
    , "message"     .= content
    ]
bufEvtPairs (StatusMessage sender content) =
    [ "event"   .= ("status" :: T.Text)
    , "sender"  .= sender
    , "message" .= content
    ]

bufEvtPairs (UserJoin user) =
    [ "event"   .= ("join" :: T.Text)
    , "user"    .= _iuNick user
    ]
bufEvtPairs (UserPart user msgM) =
    [ "event"   .= ("part" :: T.Text)
    , "user"    .= _iuNick user
    ] ++ ["message" .= fromJust msgM | isJust msgM]
bufEvtPairs (UserQuit user msgM) =
    [ "event"   .= ("quit" :: T.Text)
    , "user"    .= _iuNick user
    ] ++ ["message" .= fromJust msgM | isJust msgM]

bufEvtPairs (OtherNickChange user newNick) =
    [ "event"   .= ("other-nick" :: T.Text)
    , "user"    .= _iuNick user
    , "new-nick".= newNick
    ]


netEvtPairs :: NetworkEvent -> [Pair]
netEvtPairs (NetConnect) =
    [ "event"   .= ("connected" :: T.Text)
    ]
netEvtPairs (NetDisconnect) =
    [ "event"   .= ("disconnected" :: T.Text)
    ]
netEvtPairs (MyNickChange user newNick) =
    [ "event"   .= ("nick-change" :: T.Text)
    , "old-nick".= _iuNick user
    , "new-nick".= newNick
    ]

-- }}}

-- }}}

