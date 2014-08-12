-- | The user controller contains and manages all of the network controllers
-- and client connections for a user. It is responsible for routing events
-- between these objects.
module ChatCore.UserController
    ( UserCtlHandle
    , UserCtlActorMsg (..)
    , startUserCtl
    ) where

import Control.Applicative
import Control.Concurrent.Actor
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default
import Data.List
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Typeable
import Network

import ChatCore.Events
import ChatCore.Protocol
import ChatCore.Types
import ChatCore.Util.ActorUtil
import ChatCore.Util.StateActor

import {-# SOURCE #-} ChatCore.CoreController
import ChatCore.NetworkController

-- {{{ External Interface

-- data UserCtlHandle = UserCtlHandle
--     { ucId      :: UserId
--     , ucActor   :: Address
--     }

-- userCtlId = ucId

-- NOTE: If you change this, be sure to update the hs-boot file too.
data UserCtlActorMsg
    = UserCtlCoreEvent CoreEvent
    | UserCtlClientCommand ClientCommand
    | UserCtlNewConnection ClientConnection

instance ActorMessage UserCtlActorMsg

type UserCtlHandle = ActorHandle UserCtlActorMsg


-- | Starts a user controller for the given user ID.
startUserCtl :: UserId -> CoreCtlHandle -> IO UserCtlHandle
startUserCtl usrId coreHandle = do
    -- TODO: Look up the user in the database and load their information.
    hand <- spawnStateActor initUserCtlActor $ def { usUserId = usrId }
    return hand -- UserCtlHandle usrId addr

-- }}}

-- {{{ State and types

-- Event for adding a new connection.
data AddConnEvent = AddConn ClientConnection deriving (Typeable)


data UserCtlState = UserCtlState
    { usNetCtls :: M.Map ChatNetworkId NetCtlHandle -- Network controller handles for the user's networks.
    , usClients :: [ClientConnection] -- The clients connected as this user.
    , usUserId  :: UserId
    }

instance Default UserCtlState where
    def = UserCtlState
        { usNetCtls = M.empty
        , usClients = []
        -- The undefined should be OK in this case. If the user ID is somehow
        -- not set, the actor should crash anyway.
        , usUserId = undefined
        }

-- | State monad for the user controller actor.
type UserCtlActor = StateActorM UserCtlActorMsg UserCtlState

-- }}}

-- {{{ Other Operations

-- | Gets the user's network list from the database.
-- TODO: Implement database stuff. For now this just returns a hard coded list
-- for testing.
getNetworkList :: UserCtlActor [IRCNetwork]
getNetworkList = return
    [ IRCNetwork { inName = "EsperNet"
                 , inServers = [ IRCServer "auto" "irc.esper.net" $ PortNumber 6667 ]
                 , inNicks = ["ChatCore"]
                 , inChannels = ["#sporkk"]
                 }
    ]

-- | Starts a network controller for the given IRCNetwork.
addNetController :: IRCNetwork -> UserCtlActor ()
addNetController net = do
    me <- lift self
    -- Spawn the network controller and link to it.
    hand <- liftIO $ startNetCtl net me
    --lift $ linkActorHandle hand -- TODO: Implement linking in hactor
    modify $ \s -> do
        s { usNetCtls = M.insert (inName net) hand $ usNetCtls s }

-- | Forwards the given message to the network controller with the given ID.
-- If there is no such network, this function does nothing.
msgToNetwork :: ChatNetworkId -> NetCtlActorMsg -> UserCtlActor ()
msgToNetwork cnId msg = do
    (gets $ M.lookup cnId . usNetCtls) >>=
        (maybe (return ()) $ lift . (flip send $ msg))

-- | Forwards the client command to the network controller with the given ID.
-- If there is no such network, this function does nothing.
forwardClientCmd :: ChatNetworkId -> ClientCommand -> UserCtlActor ()
forwardClientCmd cnId ccmd = msgToNetwork cnId $ NetCtlClientCmd ccmd

-- }}}

-- {{{ Main functions

-- | Entry point for the user control actor.
-- Initializes the actor, loads the network list, and starts the network
-- controllers.
initUserCtlActor :: UserCtlActor ()
initUserCtlActor = do
    -- Start network actors.
    getNetworkList >>= mapM_ addNetController
    -- Proceed on to the main loop.
    userController

-- | The user controller actor's main function.
userController :: UserCtlActor ()
userController = do
    -- Receive the next message.
    msg <- lift receive
    case msg of
         UserCtlClientCommand ccmd -> handleClientCommand ccmd
         UserCtlCoreEvent evt -> handleCoreEvent evt
         UserCtlNewConnection conn -> handleNewConnection conn
    userController

-- }}}

-- {{{ Event handlers

-- | Handles client commands.
handleClientCommand :: ClientCommand -> UserCtlActor ()

-- Network controller commands.
handleClientCommand msg@(SendMessage netId _ _) = forwardClientCmd netId msg
handleClientCommand msg@(JoinChannel netId _)   = forwardClientCmd netId msg
handleClientCommand msg@(PartChannel netId _ _) = forwardClientCmd netId msg


-- | Handles core events from the network controller.
handleCoreEvent :: CoreEvent -> UserCtlActor ()
handleCoreEvent msg = do
    liftIO $ print msg
    gets usClients >>= (mapM_ $ \(ClientConnection conn) -> liftIO $ sendEvent conn msg)


-- | Handles connection listener events.
handleNewConnection :: ClientConnection -> UserCtlActor ()
handleNewConnection conn = do
    liftIO $ putStrLn "New connection."
    modify $ \s -> do
        s { usClients = conn : usClients s }

-- }}}

