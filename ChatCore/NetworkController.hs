{-# LANGUAGE RankNTypes #-}
-- | This is a module for handling chat sessions. It handles processing events
-- for a single user one one of that user's IRC networks.
module ChatCore.NetworkController
    ( NetCtlHandle
    , startNetCtl
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
        [ netCtlActorCase state netCtlHandleClientEvent
        , netCtlActorCase state netCtlHandleLine
        ]

netCtlActorCase :: forall m. Typeable m => NetCtlState -> (m -> NetCtlActor ()) -> Handler
netCtlActorCase state handler =
    Case $ ((flip execNetCtlActor $ state) . (>> networkController) . handler)


netCtlHandleClientEvent :: ClientEvent -> NetCtlActor ()
netCtlHandleClientEvent evt@(SendMessage _ dest msg) =
    lift2 $ print evt


netCtlHandleLine :: IRCLine -> NetCtlActor ()
-- Handle PING
netCtlHandleLine line@(IRCLine _ (IRCCommand "PING") _ addr) = do
    conn <- gets ircConnection
    lift2 $ putStrLn ("PING from " ++ show addr)
    lift2 $ doIRC conn $ sendPongCmd addr
netCtlHandleLine line = lift2 $ putStrLn ("Got unknown line: " ++ show line)


lift2 = lift . lift

