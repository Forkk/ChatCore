-- | This module provides a simple ChatCore protocol that just exposes Chat
-- Core's internal event system through a simple JSON interface.
-- It's not supported by any client, but it provides a good baseline as well as
-- a good way to test Chat Core's event system.
module ChatCore.Protocol.JSON
    ( jsonConnListener
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Error
import Control.Monad
import Control.Monad.Trans
import Data.Conduit
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time
import Data.List
import FRP.Sodium
import Network
import System.IO

import ChatCore.Events
import ChatCore.Types
import ChatCore.Util
import ChatCore.Protocol
import ChatCore.IRC
import ChatCore.ChatLog


-- | Creates a JSON connection listener listening on the given port.
jsonConnListener :: PortID -> IO ConnListener
jsonConnListener port = do
    (eNewConn, pushNewConn) <- sync newEvent
    -- Wait for a connection.
    sock <- listenOn port
    -- Accept connections.
    -- TODO: Close this thread when the program closes.
    forkIO $ forever $ do
        hand <- acceptConnection sock
        sync $ pushNewConn hand

    return ConnListener
               { clNewConn = eNewConn
               , clName = "JSON Core Protocol"
               , clId = "json"
               }


-- | Accepts a new connection on the given socket.
acceptConnection :: Socket -> IO PendingClient
acceptConnection sock = do
    liftIO $ putStrLn "JSON connection listener is awaiting a connection."
    -- Accept the sock.
    (hand, _, _) <- liftIO $ accept sock
    -- Create a pending connection from the handle.
    return $ pendingClient hand


-- | Handles authentication an initialization for the JSON protocol.
pendingClient :: Handle -> PendingClientCtx -> IO PendingClientInfo
pendingClient hand (PendingClientCtx bAuthed) = do
    putStrLn "JSON remote client pending."
    (eRecvLine, pushRecvLine) <- sync newEvent
    (eDisconnect, pushDisconnect) <- sync newEvent
    -- TODO: Handle cleanup.
    _ <- forkIO $ listenLines (sync . pushRecvLine)
                              (sync $ pushDisconnect ())
                              hand
    rec
      let eReqAuth = const ("Forkk", "testpass") <$> eRecvLine
          eAttach = const (remoteClient hand eRecvLine eDisconnect)
                    <$> gate eRecvLine bAuthed
    return $ PendingClientInfo eReqAuth eAttach


remoteClient :: Handle -> Event B.ByteString -> Event ()
             -> RemoteClientCtx -> IO RemoteClientInfo
remoteClient hand eRecvLine eDisconnect
             (RemoteClientCtx uname eCoreEvt bNetworkList) = do
    putStrLn ("JSON remote client for user " <> T.unpack uname <> " started.")
    rec
      -- To get a stream of client commands, we just parse everything from `eRecvLine`.
      let eClientMsg = filterJust ((hush . decodeClientMsg) <$> eRecvLine)
          eClientCmd = filterJust (filterChatCoreCmd <$> eClientMsg)
          filterChatCoreCmd (ChatCoreCmd cmd) = Just cmd
          filterChatCoreCmd _ = Nothing
    sync $ listen eClientMsg print
    sync $ listen eRecvLine print
    return $ RemoteClientInfo eClientCmd


-- | An IO action which listens on the given handle for newline-separated
-- messages and calls the given callback action for each line.
listenLines :: (B.ByteString -> IO ()) -- ^ Callback for receiving a line.
            -> IO () -- ^ Callback for the connection closing.
            -> Handle -> IO ()
listenLines cbkRecvLine cbkDisconnect hand = inputLoop
  where
    inputLoop = do
        -- Wait for input for 3 seconds. If no input is available, make sure the
        -- handle is still open.
        isOpen <- hIsOpen hand
        if isOpen
           then do
               B.hGetLine hand >>= cbkRecvLine
               inputLoop
           else cbkDisconnect


--------------------------------------------------------------------------------
-- JSON Protocol Messages
--------------------------------------------------------------------------------

-- | Data type for messages sent by a JSON protocol client.
data JSONClientMessage
    -- | Request @n@ many scrollback lines older than the given date.
    = GetLogLines ChatNetworkName ChatBufferName Integer UTCTime

    -------- Status Queries --------
    -- | Requests a list of the user's networks.
    | GetNetworkList
    -- | Requests information about a network.
    | GetNetworkInfo
    -- | Requests a list of the given network's buffers.
    | GetBufferList ChatNetworkName
    -- | Requests information about a specific buffer in a network.
    | GetBufferInfo ChatNetworkName ChatBufferName

    -------- Core Command --------
    -- | A client command to send to the core.
    | ChatCoreCmd ClientCommand
    deriving (Show)

-- | Data type for messages sent by the JSON protocol core.
data JSONCoreMessage
    -- | Log lines reply.
    = LogLinesReply ChatNetworkName ChatBufferName [ChatLogLine]

    -------- Status Queries --------
    -- | Network list reply.
    | NetworkListReply [ChatNetworkInfo]
    -- | Network info reply.
    | NetworkInfoReply ChatNetworkInfo
    -- | Buffer list reply.
    | BufferListReply ChatNetworkName [ChatBufferInfo]
    -- | Buffer info reply.
    | BufferInfoReply ChatNetworkName ChatBufferInfo

    -------- Core Command --------
    | ChatCoreEvent CoreEvent


-- {{{ JSON

-- {{{ JSON Parsing

decodeClientMsg :: B.ByteString -> Either String JSONClientMessage
decodeClientMsg bs = parseEither clientCmdFromJSON =<< eitherDecodeStrict bs

clientCmdFromJSON :: Value -> Parser JSONClientMessage
clientCmdFromJSON (Object obj) = do
    cmdType <- obj .: "command"
    case cmdType :: T.Text of
            "send-msg" -> ChatCoreCmd <$>
                          (SendMessage
                           <$> obj .:   "network"
                           <*> obj .:   "dest"
                           <*> obj .:   "message"
                          )
            "join-chan" -> ChatCoreCmd <$>
                           (JoinChannel
                            <$> obj .:   "network"
                            <*> obj .:   "channel"
                           )
            "part-chan" -> ChatCoreCmd <$>
                           (PartChannel
                            <$> obj .:   "network"
                            <*> obj .:   "channel"
                            <*> obj .:?  "message"
                           )
            "get-log-lines" -> GetLogLines
                                 <$> obj .: "network"
                                 <*> obj .: "buffer"
                                 <*> obj .: "line-count"
                                 <*> obj .: "start-time"
            _ -> error "Unknown command type."
clientCmdFromJSON _ =
    error "Invalid client command message. Must be an object."

-- }}}

-- {{{ JSON Serializing

encodeCoreMsg :: JSONCoreMessage -> BL.ByteString
encodeCoreMsg = encode . coreMsgToJSON

coreMsgToJSON :: JSONCoreMessage -> Value
coreMsgToJSON (ChatCoreEvent (ChatBufferEvent chatNet chatBuf evt)) = object $
    [ "network"     .= chatNet
    , "buffer"      .= chatBuf
    ] ++ bufEvtPairs evt
coreMsgToJSON (ChatCoreEvent (ChatNetworkEvent chatNet evt)) = object $
    ("network" .= chatNet) : netEvtPairs evt
coreMsgToJSON (LogLinesReply chatNet chatBuf logLines) = object
    [ "network"     .= chatNet
    , "buffer"      .= chatBuf
    , "event"       .= ("log-lines" :: T.Text)
    , "log-lines"   .= map logLineToJSON logLines
    ]


logLineToJSON :: ChatLogLine -> Value
logLineToJSON (BufLogLine _ lineTime evt) = object $
    ("time" .= lineTime) : bufEvtPairs evt

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

{-
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
             lift $ handleClientMsg handle msg
    connection handle host port
  where
    -- Parses the given message from the client.
    parseClientMsgM msgData =
        -- Log the error if the parsing failed. Otherwise, convert the
        -- Either to a Maybe and pass it on.
        logParseError $ decodeClientMsg msgData
    -- If the given argument is a parse error message, log it.
    logParseError (Right cmd) =
        return $ Just cmd
    logParseError (Left errMsg) = do
        liftIO $ putStrLn ("Error parsing JSON message: " ++ errMsg)
        return Nothing

-- }}}



-- | Handles a message from the client.
handleClientMsg :: Handle -> JSONClientMessage -> RemoteClient ()
handleClientMsg _ (ChatCoreCmd cmd) = receivedClientCmd cmd

handleClientMsg handle (GetLogLines netName bufName lineCount startTime) = do
    uctl <- getUserCtl
    -- Get the network controller's handle.
    nctlM <- ucGetNetCtl uctl netName
    unless (isNothing nctlM) $ do
        let nctl = fromJust nctlM
        chatLog <- ncGetChatLog nctl
        logLines <- genericTake lineCount <$> liftIO (readLog chatLog bufName startTime)
        -- Send the lines to the user.
        sendCoreMsg handle $ LogLinesReply netName bufName logLines


handleClientMsg handle GetNetworkList = do
    uctl <- getUserCtl
    netInfos <- ucGetNetList
    sendCoreMsg handle $ NetworkListReply netInfos

-- | GetNetworkList
-- -- | Requests information about a network.
-- | GetNetworkInfo
-- -- | Requests a list of the given network's buffers.
-- | GetBufferList ChatNetworkName
-- -- | Requests information about a specific buffer in a network.
-- | GetBufferInfo ChatNetworkName ChatBufferName

-- | Handles a core event.
handleCoreEvt :: Handle -> CoreEvent -> RemoteClient ()
handleCoreEvt handle evt =
    sendCoreMsg handle $ ChatCoreEvent evt


-- | Sends a JSONCoreMessage.
sendCoreMsg :: Handle -> JSONCoreMessage -> RemoteClient ()
sendCoreMsg handle msg =
    liftIO $ TL.hPutStrLn handle $ TL.decodeUtf8 $ encodeCoreMsg msg


-}
