-- | The user controller contains and manages all of the network controllers
-- and client connections for a user. It is responsible for routing events
-- between these objects.
module ChatCore.UserController
    ( UserCtlHandle
    , startUserCtl
    , userCtlId

    , AddConnEvent (AddConn)
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
import ChatCore.NetworkController
import ChatCore.Protocol
import ChatCore.Types
import ChatCore.Util.ActorUtil
import ChatCore.Util.StateActor

-- {{{ External Interface

data UserCtlHandle = UserCtlHandle
    { ucId      :: UserId
    , ucActor   :: Address
    }

userCtlId = ucId

instance ActorHandle UserCtlHandle where
    actorAddr = ucActor

-- | Starts a user controller for the given user ID.
startUserCtl :: UserId -> CoreCtlHandle -> IO UserCtlHandle
startUserCtl usrId coreHandle = do
    -- TODO: Look up the user in the database and load their information.
    addr <- spawnStateActor initUserCtlActor $ def { usUserId = usrId }
    return $ UserCtlHandle usrId addr

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
type UserCtlActor = StateActor UserCtlState
type UserCtlActorM = StateActorM UserCtlState

-- }}}

-- {{{ Other Operations

-- | Gets the user's network list from the database.
-- TODO: Implement database stuff. For now this just returns a hard coded list
-- for testing.
getNetworkList :: UserCtlActorM [IRCNetwork]
getNetworkList = return
    [ IRCNetwork { inName = "EsperNet"
                 , inServers = [ IRCServer "auto" "irc.esper.net" $ PortNumber 6667 ]
                 , inNicks = ["ChatCore"]
                 , inChannels = ["#sporkk"]
                 }
    ]

-- | Starts a network controller for the given IRCNetwork.
addNetController :: IRCNetwork -> UserCtlActor
addNetController net = do
    me <- lift self
    -- Spawn the network controller and link to it.
    hand <- lift2 $ startNetCtl net me
    lift $ linkActorHandle hand
    modify $ \s -> do
        s { usNetCtls = M.insert (netCtlId hand) hand $ usNetCtls s }

-- | Forwards the given message to the network controller with the given ID.
-- If there is no such network, this function does nothing.
msgToNetwork :: (Typeable msg, Show msg) => ChatNetworkId -> msg -> UserCtlActor
msgToNetwork cnId msg = do
    -- TODO: The network handle list should probably be a dictionary.
    -- It shouldn't matter too much though, since it shouldn't ever be a very large list.
    lift2 $ putStrLn ("Sending message to network controller: " ++ show msg)
    (gets $ M.lookup cnId . usNetCtls) >>=
        (maybe (return ()) $ lift . (flip sendActorMsg $ msg))

-- }}}

-- {{{ Main functions

-- | Entry point for the user control actor.
-- Initializes the actor, loads the network list, and starts the network
-- controllers.
initUserCtlActor :: UserCtlActor
initUserCtlActor = do
    -- Start network actors.
    getNetworkList >>= mapM_ addNetController
    -- Proceed on to the main loop.
    userController

-- | The user controller actor's main function.
userController :: UserCtlActor
userController = do
    -- Get the state.
    state <- get
    -- Make a function to wrap handlers.
    let handler :: forall m. Typeable m => (m -> UserCtlActor) -> Handler
        handler = stateActorHandler userController state
    lift $ receive $
        [ handler handleClientCommand
        , handler handleCoreEvent
        , handler handleNewConnEvent
        ]

-- }}}

-- {{{ Event handlers

-- | Handles client commands.
handleClientCommand :: ClientCommand -> UserCtlActor

-- Network controller commands.
handleClientCommand msg@(SendMessage netId _ _) = msgToNetwork netId msg
handleClientCommand msg@(JoinChannel netId _)   = msgToNetwork netId msg
handleClientCommand msg@(PartChannel netId _ _) = msgToNetwork netId msg


-- | Handles core events from the network controller.
handleCoreEvent :: CoreEvent -> UserCtlActor
handleCoreEvent msg = do
    lift2 $ print msg
    gets usClients >>= (mapM_ $ \(ClientConnection conn) -> lift2 $ sendEvent conn msg)


-- | Handles connection listener events.
handleNewConnEvent :: AddConnEvent -> UserCtlActor
handleNewConnEvent (AddConn conn) = do
    liftIO $ putStrLn "New connection."
    modify $ \s -> do
        s { usClients = conn : usClients s }

-- }}}

-- {{{ Utility functions

lift2 = lift . lift

-- }}}

