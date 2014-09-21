-- | The core controller is ChatCore's main actor. It keeps track of the user
-- controllers and the protocol controller.
module ChatCore.CoreController
    ( CoreCtlHandle
    , CoreActorMsg (..)
    , runCoreCtl
    ) where

import Control.Applicative
import Control.Concurrent.Actor
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.State
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default
import qualified Data.Map as M
import Network

import ChatCore.Protocol
import ChatCore.Protocol.JSON
import ChatCore.Types
import ChatCore.UserController

-- {{{ State and types

data CoreCtlState = CoreCtlState
    { _ccUserCtls :: M.Map UserId UserCtlHandle -- User controller handles.
    , _ccConnListeners :: [ActorHandle ()]
    , _ccPendingConns :: [Async (UserId, RemoteClient ())]
    }
makeLenses ''CoreCtlState

instance Default CoreCtlState where
    def = CoreCtlState
        { _ccUserCtls = M.empty
        , _ccConnListeners = []
        , _ccPendingConns = []
        }

-- | Monad constraint for the core control actor.
type CoreCtlActor m = (MonadActor CoreActorMsg m, MonadState CoreCtlState m)

-- }}}

-- {{{ External interface

data CoreActorMsg
    = CoreNewConnection PendingConn

instance ActorMessage CoreActorMsg

type CoreCtlHandle = ActorHandle CoreActorMsg

-- | Starts a core controller on the current thread.
-- This does **not** fork to another thread.
runCoreCtl :: IO ()
runCoreCtl = runActor $ evalStateT initCoreCtlActor def

-- }}}

-- {{{ Other Operations

-- | Gets the user list from the database.
-- TODO: Implement database stuff. For now this just returns a hard coded list
-- for testing.
getUserList :: (CoreCtlActor m) => m [UserId]
getUserList = return [ "Forkk" ]


getConnListeners :: (CoreCtlActor m) => m [ConnListener]
getConnListeners = return
    [ jsonConnListener $ PortNumber 1337
    ]


-- | Starts a user controller for the given IRCUser.
addUserController :: (CoreCtlActor m) => UserId -> m ()
addUserController userId = do
    me <- self
    -- Spawn the user controller and link to it.
    hand <- liftIO $ startUserCtl userId me
    -- linkActorHandle hand -- TODO: Implement linking in hactor
    ccUserCtls %= M.insert userId hand


-- | Starts the given connection listener.
startConnListener :: (CoreCtlActor m) => ConnListener -> m ()
startConnListener ct = do
    me <- self
    hand <- liftIO $ spawnConnListener me ct
    -- linkActorHandle hand
    ccConnListeners %= (hand :)


type ConnListenerHandle = ActorHandle ()

-- | Starts a connection listener for the given core type and returns a handle.
spawnConnListener :: CoreCtlHandle -> ConnListener -> IO (ConnListenerHandle)
spawnConnListener coreCtl cl = do
    hand <- spawnActor $ runConnListener coreCtl cl
    return hand

-- | Actor which runs a connection listener.
runConnListener :: CoreCtlHandle -> ConnListener -> ActorM () ()
runConnListener coreCtl cl =
    liftIO (listenerFunc cl $$ CL.mapM_ (send coreCtl . CoreNewConnection))

-- }}}

-- {{{ Main functions

-- | Entry point for the core controller actor.
-- Initializes the actor, loads the user list, and starts the user controllers.
initCoreCtlActor :: StateT CoreCtlState (ActorM CoreActorMsg) ()
initCoreCtlActor = do
    -- Start user controllers.
    getUserList >>= mapM_ addUserController
    -- Start connection listeners.
    getConnListeners >>= mapM_ startConnListener
    -- Proceed on to the main loop.
    coreController

-- | The core controller actor's main function.
coreController :: (CoreCtlActor m) => m ()
coreController = do
    stmRcv <- receiveSTM
    pcs <- use ccPendingConns
    val <- liftIO $ atomically $
        (Left  <$> waitAnySTM pcs)
         `orElse`
        (Right <$> stmRcv)
    case val of
         Right msg -> handleMessage msg
         Left conn -> handleCompleteConn conn
    coreController

-- | Handles a received message.
handleMessage :: (CoreCtlActor m) => CoreActorMsg -> m ()
handleMessage (CoreNewConnection conn) = handlePendingConn conn


-- }}}

-- {{{ Event handlers and commands

-- | Handles new pending connections.
handlePendingConn :: (CoreCtlActor m) => PendingConn -> m ()
handlePendingConn pc = do
    me <- self
    -- Start an Async thread for the connection.
    -- TODO: Clean up these threads if the core controller crashes.
    thread <- liftIO $ async $ pc me
    -- Add the async thread to our pending connections list.
    ccPendingConns %= (thread :) -- Lenses make me smile..


type CompleteConn = (UserId, RemoteClient ())

-- | Handles a completed pending connection.
handleCompleteConn :: (CoreCtlActor m) => (Async CompleteConn, CompleteConn) -> m ()
handleCompleteConn (thread, (user, rc)) = do
    -- Remove the pending thread.
    ccPendingConns %= filter (/= thread)
    -- Find the user controller we're attaching to.
    userCtlM <- use (ccUserCtls . at user)
    -- Send the client to the user controller.
    case userCtlM of
         Just userCtl -> ucSendNewClient userCtl rc
         Nothing -> return ()

-- }}}

-- {{{ Utility functions

-- | Waits for any of the given async operations to finish in the STM monad.
waitAnySTM :: [Async a] -> STM (Async a, a)
waitAnySTM threads =
    foldr orElse retry $ map (\a -> waitSTM a >>= return . (a,)) threads

-- }}}

