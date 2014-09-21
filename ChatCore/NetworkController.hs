{-# LANGUAGE UndecidableInstances #-}
-- | This is a module for handling chat sessions. It handles processing events
-- for a single user one one of that user's IRC networks.
module ChatCore.NetworkController
    ( NetCtlHandle
    , NetCtlActorMsg (..)
    , startNetCtl
    ) where

import Control.Concurrent.Actor
import Control.Concurrent.Async
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Typeable
import Network
import System.FilePath

import ChatCore.Events
import ChatCore.IRC
import ChatCore.IRC.Commands
import ChatCore.Protocol
import ChatCore.Types
import ChatCore.ChatLog
import ChatCore.ChatLog.Line
import ChatCore.Util.StateActor
import {-# SOURCE #-} ChatCore.UserController

-- {{{ State and types

-- | Data structure which stores the state of a chat session on a network.
data NetCtlState = NetCtlState
    { _nsId         :: ChatNetworkId    -- The ID name of this network.
    , _netNick      :: Nick             -- The user's nick.
    , _netChannels  :: [ChatChan]       -- A list of channels the current user is in.
    , _netHost      :: HostName
    , _netPort      :: PortID
    , _netIRCConn   :: IRCConnection
    , _userCtlAddr  :: UserCtlHandle    -- The user controller.
    , _netChatLog   :: ChatLog          -- The chat log for this network.
    , _netRecvAsync :: Async ()         -- Async thread for receiving messages.
    }

makeLenses ''NetCtlState

-- | Monad constraint type for the network controller actor.
type NetCtlActor m =
    ( MonadActor NetCtlActorMsg m
    , MonadState NetCtlState m
    , MonadResource m, MonadIRC m)

instance (NetCtlActor m) => MonadIRC m where
    ircConn = use netIRCConn

-- }}}

-- {{{ External Interface

-- | A handle pointing to a running network controller.
-- data NetCtlHandle = NetCtlHandle
--     { netCtlId  :: ChatNetworkId    -- The ID of the controller's network.
--     , netActor  :: Address          -- The address of the controller's actor.
--     }

-- NOTE: If you change this, be sure to update the hs-boot file too.
data NetCtlActorMsg
    = NetCtlClientCmd   ClientCommand
    | NetCtlIRCLine     IRCLine

instance ActorMessage NetCtlActorMsg

type NetCtlHandle = ActorHandle NetCtlActorMsg

startNetCtl :: IRCNetwork -> UserCtlHandle -> IO NetCtlHandle
startNetCtl ircNet ucAddr = do
    -- Open the chat log for this network.
    chatLog <- mkChatLog ("log" </> (T.unpack $ inName ircNet))
    -- Connect to the first server.
    -- TODO: Implement connecting to other servers in the list.
    let host = servAddress $ head $ inServers ircNet
        port = servPort $ head $ inServers ircNet
    hand <- spawnActor $ initNetCtlActor $ NetCtlState
        { _nsId = inName ircNet
        , _netNick = head $ inNicks ircNet
        , _netChannels = inChannels ircNet
        , _netHost = host
        , _netPort = port
        -- Will connect on startup.
        , _netIRCConn = undefined
        , _netRecvAsync = undefined
        , _userCtlAddr = ucAddr
        , _netChatLog = chatLog
        }
    return hand

-- }}}

-- {{{ Main functions

-- | Initializes a network controller with the given state.
initNetCtlActor :: NetCtlState -> ActorM NetCtlActorMsg ()
initNetCtlActor = evalStateT $ runResourceT $ do
    liftIO $ putStrLn "Connecting to IRC..."
    -- Connect to IRC.
    host <- use netHost
    port <- use netPort
    (_, conn) <- allocate
        (connectIRC host port)
        disconnectIRC
    netIRCConn .= conn
    liftIO $ putStrLn "IRC connection established."

    -- Start the receiver thread.
    me <- self
    recvSrc <- sourceRecvLine
    (_, rth) <- allocate
        (async (recvSrc $$ CL.mapM_ (send me . NetCtlIRCLine)))
        cancel
    netRecvAsync .= rth

    -- Initialize the IRC connection.
    sendNickCmd =<< use netNick
    (flip sendUserCmd) "Chat Core" =<< use netNick
    -- Start the main function.
    networkController

-- | The actual network controller actor.
networkController :: (NetCtlActor m) => m ()
networkController = do
    -- Read a message.
    msg <- receive
    case msg of
         NetCtlClientCmd ccmd -> handleClientCmd ccmd
         NetCtlIRCLine line -> handleLine line
    networkController

-- }}}

-- {{{ Utility functions

-- | Sends the given core event to the user controller.
ncSendCoreEvent :: (NetCtlActor m) => CoreEvent -> m ()
ncSendCoreEvent evt = do
    uc <- use userCtlAddr
    ucSendCoreEvt uc evt

-- | Sends a buffer event for the given buffer.
bufferEvent :: (NetCtlActor m) => ChatBufferId -> BufferEvent -> m ()
bufferEvent bufId evt = do
    -- Get the network ID.
    netId <- use nsId
    -- Send the event and log it.
    ncSendCoreEvent $ BufCoreEvent netId bufId evt
    logEvent bufId $ LogBufEvent evt

-- | Logs the given event.
logEvent :: (NetCtlActor m) => BufferId -> LogEvent -> m ()
logEvent buf evt = do
    time <- liftIO getCurrentTime
    chatLog <- use netChatLog
    let logLine = LogLine buf time evt
    liftIO $ writeLogLine chatLog logLine

-- }}}

-- {{{ Handler functions

-- | Handles a client event for the given network controller.
handleClientCmd :: (NetCtlActor m) => ClientCommand -> m ()

handleClientCmd (JoinChannel _ chan) = sendJoinCmd chan
handleClientCmd (PartChannel _ chan msg) =
    sendPartCmd chan $ fromMaybe "Leaving" msg

handleClientCmd (SendMessage _ dest msg MtPrivmsg) =
    sendPrivMsgCmd dest msg
handleClientCmd (SendMessage _ dest msg MtNotice) =
    sendNoticeCmd dest msg



-- | Handles an IRC line.
handleLine :: (NetCtlActor m) => IRCLine -> m ()
-- Handle PING
handleLine (IRCLine _ (ICmdPing) _ addr) = do
    sendPongCmd addr

-- PRIVMSG and NOTICE
handleLine (IRCLine (Just sender) (ICmdPrivmsg) [source] (Just msg)) = do
    bufferEvent source $ ReceivedMessage sender msg MtPrivmsg
handleLine (IRCLine (Just sender) (ICmdNotice) [source] (Just msg)) = do
    bufferEvent source $ ReceivedMessage sender msg MtNotice

-- JOIN, PART, and QUIT
handleLine (IRCLine (Just user) (ICmdJoin) [chan] Nothing) = do
    bufferEvent chan $ UserJoin user
handleLine (IRCLine (Just user) (ICmdPart) [chan] msgM) = do
    bufferEvent chan $ UserPart user msgM
handleLine (IRCLine (Just user) (ICmdQuit) [chan] msgM) = do
    bufferEvent chan $ UserQuit user msgM


handleLine line =
    liftIO $ putStrLn ("Got unknown line: " ++ show line)

-- }}}

