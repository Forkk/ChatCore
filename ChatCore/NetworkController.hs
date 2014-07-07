{-# LANGUAGE RankNTypes #-}
-- | This is a module for handling chat sessions. It handles processing events
-- for a single user one one of that user's IRC networks.
module ChatCore.NetworkController
    ( NetCtlHandle
    , startNetCtl
    , sendNetCtl
    ) where

import Control.Concurrent.Actor
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.Typeable
import Network

import ChatCore.Protocol
import ChatCore.Events
import ChatCore.IRC
import ChatCore.IRC.Commands
import ChatCore.Types

-- | A handle pointing to a running network controller.
data NetCtlHandle = NetCtlHandle
    { netId         :: ChatNetworkId    -- The ID of the controller's network.
    , netActorAddr  :: Address          -- The address of the controller's actor.
    }

startNetCtl :: ChatNetworkId -> IO NetCtlHandle
startNetCtl cnId = do
    let host = "irc.esper.net"
        port = PortNumber 6667
    connection <- connectIRC host port
    addr <- spawn $ netCtlActor $ NetCtlState
        { nsId = cnId
        , netNick = "ChatCore"
        , netChannels = []
        , netAddress = host
        , netPort = port
        , ircConnection = connection
        }
    return $ NetCtlHandle
        { netId = cnId
        , netActorAddr = addr
        }

-- | Sends a client event to the given network controller from outside the
-- context of an actor.
-- This is done asynchronously.
sendNetCtl :: NetCtlHandle -> ClientCommand -> IO ()
sendNetCtl handle evt = do
    spawn $ send (netActorAddr handle) evt
    return ()


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
type NetCtlActor = StateT NetCtlState ActorM

-- | Executes a NetCtlActor monad with the given state and returns the modified state.
-- Any return value from the NetCtlActor is discarded.
execNetCtlActor :: NetCtlActor a -> NetCtlState -> ActorM a
execNetCtlActor = evalStateT


-- | An actor spawned by the network controller which receives messages from
-- the IRC connection and sends them as actor messages to the network
-- controller.
receiveActor :: Address -> IRCConnection -> Actor
receiveActor ncActor conn = do
    line <- lift $ evalIRCAction recvLine conn
    send ncActor line
    receiveActor ncActor conn


-- | Executes a network controller with the given state.
netCtlActor :: NetCtlState -> Actor
netCtlActor state = do
    me <- self
    recvActor <- lift $ spawn $ receiveActor me $ ircConnection state
    link recvActor

    -- Connect to the network.
    lift $ doIRC (ircConnection state) $ do
        sendNickCmd $ netNick state
        sendUserCmd (netNick state) "Chat Core"

    execNetCtlActor networkController state

networkController :: NetCtlActor ()
networkController = do
    me <- lift self
    state <- get
    lift $ receive $
        [ netCtlActorCase state netCtlHandleClientCommand
        , netCtlActorCase state netCtlHandleLine
        ]

netCtlActorCase :: forall m. Typeable m => NetCtlState -> (m -> NetCtlActor ()) -> Handler
netCtlActorCase state handler =
    Case $ ((flip execNetCtlActor $ state) . (>> networkController) . handler)


-- | Executes an IRC action from within a NetCtlActor context.
ncIRC :: IRC a -> NetCtlActor a
ncIRC action = gets ircConnection >>= (lift2 . evalIRCAction action)


-- | Handles a client event for the given network controller.
netCtlHandleClientCommand :: ClientCommand -> NetCtlActor ()

netCtlHandleClientCommand (JoinChannel _ chan) = ncIRC $ sendJoinCmd chan
netCtlHandleClientCommand (PartChannel _ chan msg) = ncIRC $ sendPartCmd chan msg

netCtlHandleClientCommand (SendMessage _ dest msg) = ncIRC $ sendPrivMsgCmd dest msg

netCtlHandleClientCommand evt = lift2 $ print evt



-- | Handles an IRC line.
netCtlHandleLine :: IRCLine -> NetCtlActor ()
-- Handle PING
netCtlHandleLine line@(IRCLine _ (IRCCommand "PING") _ addr) = do
    lift2 $ putStrLn ("PING from " ++ show addr)
    ncIRC $ sendPongCmd addr

netCtlHandleLine line = lift2 $ putStrLn ("Got unknown line: " ++ show line)


lift2 = lift . lift

