-- | This module provides a simple ChatCore protocol that just exposes Chat
-- Core's internal event system through a simple JSON interface.
-- It's not supported by any client, but it provides a good baseline as well as
-- a good way to test Chat Core's event system.
module ChatCore.ClientProtocol.JSON
    ( jsonConnListener
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Error
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Trans
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
import FRP.Sodium
import FRP.Sodium.IO
import Network
import System.IO

import ChatCore.ChatBuffer
import ChatCore.Events
import ChatCore.Types
import ChatCore.ClientProtocol
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
    _ <- forkIO $ forever $ do
                          client <- acceptConnection sock
                          sync $ pushNewConn client
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
    pendingClient hand


-- | Handles authentication an initialization for the JSON protocol.
pendingClient :: Handle -> IO (PendingClientCtx -> PendingClientInfo)
pendingClient hand = do
    (eRecvLine, pushRecvLine) <- sync newEvent
    (eDisconnect, pushDisconnect) <- sync newEvent
    -- TODO: Clean up this thread.
    _ <- forkIO $ listenLines (sync . pushRecvLine)
                              (sync $ pushDisconnect ())
                              hand
    putStrLn "JSON remote client pending."
    sync $ listen eRecvLine print
    return $ \(PendingClientCtx bAuthed) ->
      let eReqAuth = const ("Forkk", "testpass") <$> eRecvLine
          eAttach = const (remoteClient hand eRecvLine eDisconnect)
                    <$> gate eRecvLine bAuthed
          in PendingClientInfo eReqAuth eAttach


remoteClient :: Handle -> Event B.ByteString -> Event ()
             -> RemoteClientCtx -> IO RemoteClientInfo
remoteClient hand eRecvLine eDisconnect
             (RemoteClientCtx uName eCoreEvt bNetworks) = do
    putStrLn ("JSON remote client for user " <> T.unpack uName <> " started.")
    rec
      --------------------------------------------------------------------------------
      -- Client Messages
      --------------------------------------------------------------------------------

      -- To get a stream of client commands, we just parse everything from `eRecvLine`.
      let eClientMsg :: Event JSONClientMessage
          eClientMsg = filterJust ((hush . decodeClientMsg) <$> eRecvLine)

      let eClientCmd :: Event ClientCommand
          eClientCmd = filterJust (filterChatCoreCmd <$> eClientMsg)
          filterChatCoreCmd (ChatCoreCmd cmd) = Just cmd
          filterChatCoreCmd _ = Nothing

      
      -- An event containing log events requested by the client.
      let eLogLineMsgs :: Event JSONCoreMessage
          eLogLineMsgs = executeAsyncIO (getRequestedLogs <$> eLogReqBufs)
          getRequestedLogs (netName, buf, lcount, stime) =
              LogLinesReply netName bufName <$> getBufLogLines buf lcount stime
            where
              bufName = buf ^. bufferName
          
      -- Log line requests.
      let eLogReqs :: Event (ChatNetworkName, ChatBufferName, Integer, UTCTime)
          eLogReqs = filterJust (mkLogReq <$> eClientMsg)
          mkLogReq (GetLogLines netName bufName lcount startTime) =
              Just (netName, bufName, lcount, startTime)
          mkLogReq _ = Nothing
      
      -- Snapshots the networks specified in the given log requests.
      let eLogReqBufs :: Event (ChatNetworkName, ChatBuffer, Integer, UTCTime)
          eLogReqBufs = filterJust $ execute (lookupBufs <$> eLogReqs)
          lookupBufs (netName, bufName, lineCount, startTime) = runMaybeT $ do
              buf <- MaybeT $ getUserBuf bNetworks netName bufName
              return (netName, buf, lineCount, startTime)

      --------------------------------------------------------------------------------
      -- Sending
      --------------------------------------------------------------------------------
      
      let eSendCoreMsg = eCoreEvtMsg <> eLogLineMsgs
      let eCoreEvtMsg = ChatCoreEvent <$> eCoreEvt
      
    cleanSendCoreMsg <- sync $ listen eSendCoreMsg $
                        TL.hPutStrLn hand . TL.decodeUtf8 . encodeCoreMsg

    let clean = do
          cleanSendCoreMsg
          hClose hand

    return $ RemoteClientInfo eClientCmd eDisconnect clean


-- | An IO action which listens on the given handle for newline-separated
-- messages and calls the given callback action for each line.
listenLines :: (B.ByteString -> IO ()) -- ^ Callback for receiving a line.
            -> IO () -- ^ Callback for the connection closing.
            -> Handle -> IO ()
listenLines cbkRecvLine cbkDisconnect hand = inputLoop
  where
    inputLoop =
      -- Attempt to receive input. If an exception occurs, trigger the
      -- disconnect callback and stop.
      catch (B.hGetLine hand >>= cbkRecvLine >> inputLoop)
            (\e -> do
               putStrLn ("Remote connection error: "
                         <> show (e :: IOException))
               cbkDisconnect)


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
    deriving (Show)


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
coreMsgToJSON _ = object []


logLineToJSON :: ChatLogLine -> Value
logLineToJSON (BufLogLine lineTime evt) = object $
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
