{-# LANGUAGE RankNTypes #-}
-- | This is a module for handling chat sessions. It handles processing events
-- for a single user one one of that user's IRC networks.
module ChatCore.NetworkController
    ( NetCtlHandle
    , startNetCtl

    , netCtlId
    ) where

import Control.Concurrent.Actor
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.Typeable
import Network

import ChatCore.Events
import ChatCore.IRC
import ChatCore.IRC.Commands
import ChatCore.Protocol
import ChatCore.Types
import ChatCore.Util.ActorUtil
import ChatCore.Util.StateActor

-- {{{ External Interface

-- | A handle pointing to a running network controller.
data NetCtlHandle = NetCtlHandle
    { netCtlId  :: ChatNetworkId    -- The ID of the controller's network.
    , netActor  :: Address          -- The address of the controller's actor.
    }

instance ActorHandle NetCtlHandle where
    actorAddr = netActor

startNetCtl :: IRCNetwork -> IO NetCtlHandle
startNetCtl ircNet = do
    let host = servAddress $ head $ inServers ircNet
        port = servPort $ head $ inServers ircNet
    connection <- connectIRC host port
    addr <- spawn $ initNetCtlActor $ NetCtlState
        { nsId = inName ircNet
        , netNick = head $ inNicks ircNet
        , netChannels = inChannels ircNet
        -- TODO: Server list stuff.
        , netAddress = host
        , netPort = port
        , ircConnection = connection
        }
    return $ NetCtlHandle (inName ircNet) addr

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
    }

-- | State monad for the network controller actor.
type NetCtlActor = StateActor NetCtlState
type NetCtlActorM = StateActorM NetCtlState

-- }}}

-- | An actor spawned by the network controller which receives messages from
-- the IRC connection and sends them as actor messages to the network
-- controller.
receiveActor :: Address -> IRCConnection -> Actor
receiveActor ncActor conn = do
    line <- lift $ evalIRCAction recvLine conn
    send ncActor line
    receiveActor ncActor conn


-- | Initializes a network controller with the given state.
initNetCtlActor :: NetCtlState -> Actor
initNetCtlActor state = do
    -- Spawn the receiver actor.
    me <- self
    recvActor <- lift $ spawn $ receiveActor me $ ircConnection state
    link recvActor

    -- Connect to the network.
    lift $ doIRC (ircConnection state) $ do
        sendNickCmd $ netNick state
        sendUserCmd (netNick state) "Chat Core"

    -- Start the network controller's state actor.
    runStateActor networkController state

-- | The actual network controller actor.
networkController :: NetCtlActor
networkController = do
    state <- get
    let handler :: forall m. Typeable m => (m -> NetCtlActor) -> Handler
        handler = stateActorHandler networkController state
    lift $ receive $
        [ handler netCtlHandleClientCommand
        , handler netCtlHandleLine
        ]


-- | Executes an IRC action from within a NetCtlActor context.
ncIRC :: IRC a -> NetCtlActorM a
ncIRC action = gets ircConnection >>= (lift . lift . evalIRCAction action)


-- | Handles a client event for the given network controller.
netCtlHandleClientCommand :: ClientCommand -> NetCtlActor

netCtlHandleClientCommand (JoinChannel _ chan) = ncIRC $ sendJoinCmd chan
netCtlHandleClientCommand (PartChannel _ chan msg) = ncIRC $ sendPartCmd chan msg

netCtlHandleClientCommand (SendMessage _ dest msg) = ncIRC $ sendPrivMsgCmd dest msg

netCtlHandleClientCommand evt = lift2 $ print evt



-- | Handles an IRC line.
netCtlHandleLine :: IRCLine -> NetCtlActor
-- Handle PING
netCtlHandleLine line@(IRCLine _ (IRCCommand "PING") _ addr) = do
    lift2 $ putStrLn ("PING from " ++ show addr)
    ncIRC $ sendPongCmd addr

netCtlHandleLine line = lift2 $ putStrLn ("Got unknown line: " ++ show line)

lift2 = lift . lift

