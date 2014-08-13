{-# LANGUAGE RankNTypes #-}
-- | This is a module for handling chat sessions. It handles processing events
-- for a single user one one of that user's IRC networks.
module ChatCore.NetworkController
    ( NetCtlHandle
    , NetCtlActorMsg (..)
    , startNetCtl
    ) where

import Control.Concurrent.Actor
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Network

import ChatCore.Events
import ChatCore.IRC
import ChatCore.IRC.Commands
import ChatCore.Protocol
import ChatCore.Types
import ChatCore.Util.ActorUtil
import ChatCore.Util.StateActor
import {-# SOURCE #-} ChatCore.UserController

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
    let host = servAddress $ head $ inServers ircNet
        port = servPort $ head $ inServers ircNet
    connection <- connectIRC host port
    hand <- spawnActor $ initNetCtlActor $ NetCtlState
        { nsId = inName ircNet
        , netNick = head $ inNicks ircNet
        , netChannels = inChannels ircNet
        -- TODO: Server list stuff.
        , netAddress = host
        , netPort = port
        , ircConnection = connection
        , userCtlAddr = ucAddr
        }
    return hand -- NetCtlHandle (inName ircNet) addr

-- }}}

-- {{{ State and types

-- | Data structure which stores the state of a chat session on a network.
data NetCtlState = NetCtlState
    { nsId          :: ChatNetworkId    -- The ID name of this network.
    , netNick       :: Nick             -- The current user's nick.
    , netChannels   :: [ChatChan]       -- A list of channels the current user is in.
    , netAddress    :: HostName
    , netPort       :: PortID
    , ircConnection :: IRCConnection    -- The IRC connection.
    , userCtlAddr   :: UserCtlHandle    -- The user controller.
    }

-- | State monad for the network controller actor.
type NetCtlActor = StateActorM NetCtlActorMsg NetCtlState

-- }}}

-- {{{ Main functions

-- | An actor spawned by the network controller which receives messages from
-- the IRC connection and sends them as actor messages to the network
-- controller.
receiveActor :: NetCtlHandle -> IRCConnection -> ActorM () ()
receiveActor ncActor conn = do
    line <- lift $ evalIRCAction recvLine conn
    send ncActor $ NetCtlIRCLine line
    receiveActor ncActor conn


-- | Initializes a network controller with the given state.
initNetCtlActor :: NetCtlState -> ActorM NetCtlActorMsg ()
initNetCtlActor state = do
    -- Spawn the receiver actor.
    me <- self
    recvActor <- lift $ spawnActor $ receiveActor me $ ircConnection state
    --link recvActor -- TODO: Implement linking in hactor

    -- Connect to the network.
    lift $ doIRC (ircConnection state) $ do
        sendNickCmd $ netNick state
        sendUserCmd (netNick state) "Chat Core"

    -- Start the network controller's state actor.
    runStateActor networkController state

-- | The actual network controller actor.
networkController :: NetCtlActor ()
networkController = do
    -- Read a message.
    msg <- lift receive
    case msg of
         NetCtlClientCmd ccmd -> handleClientCmd ccmd
         NetCtlIRCLine line -> handleLine line
    networkController

-- }}}

-- {{{ Utility functions

-- | Executes an IRC action from within a NetCtlActor context.
ncIRC :: IRC a -> NetCtlActor a
ncIRC action = gets ircConnection >>= (lift . lift . evalIRCAction action)

-- | Sends the given core event to the user controller.
sendCoreEvent :: CoreEvent -> NetCtlActor ()
sendCoreEvent evt = do
    gets userCtlAddr >>= lift . (flip send $ UserCtlCoreEvent $ evt)

-- }}}

-- {{{ Handler functions

-- | Handles a client event for the given network controller.
handleClientCmd :: ClientCommand -> NetCtlActor ()

handleClientCmd (JoinChannel _ chan) = ncIRC $ sendJoinCmd chan
handleClientCmd (PartChannel _ chan msg) =
    ncIRC $ sendPartCmd chan $ fromMaybe "Leaving" msg

handleClientCmd (SendMessage _ dest msg) = ncIRC $ sendPrivMsgCmd dest msg



-- | Handles an IRC line.
handleLine :: IRCLine -> NetCtlActor ()
-- Handle PING
handleLine (IRCLine _ (IRCCommand "PING") _ addr) = do
    ncIRC $ sendPongCmd addr

-- PRIVMSG and NOTICE
handleLine (IRCLine (Just sender) (IRCCommand "PRIVMSG") [source] (Just msg)) = do
    netid <- gets nsId
    sendCoreEvent $ ReceivedMessage
        { recvMsgNetwork = netid
        , recvMsgSource = source
        , recvMsgSender = sender
        , recvMsgContent = msg
        }

handleLine line =
    return ()
    -- liftIO $ putStrLn ("Got unknown line: " ++ show line)

-- }}}

