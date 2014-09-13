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
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Typeable
import Network

import ChatCore.Protocol
import ChatCore.Protocol.JSON
import ChatCore.Protocol.Quassel
import ChatCore.Types
import ChatCore.UserController
import ChatCore.Util.StateActor


-- {{{ External interface

data CoreActorMsg
    = CoreNewConnection PendingConn

instance ActorMessage CoreActorMsg

type CoreCtlHandle = ActorHandle CoreActorMsg

-- | Starts a core controller on the current thread.
-- This does **not** fork to another thread.
runCoreCtl :: IO ()
runCoreCtl = runActor $ runStateActor initCoreCtlActor $ def

-- }}}

-- {{{ State and types

data CoreCtlState = CoreCtlState
    { ccUserCtls :: M.Map UserId UserCtlHandle -- User controller handles.
    , ccConnListeners :: [ConnListenerHandle]
    , ccPendingConns :: [Async (UserId, RemoteClient ())]
    }

instance Default CoreCtlState where
    def = CoreCtlState
        { ccUserCtls = M.empty
        , ccConnListeners = []
        , ccPendingConns = []
        }

-- | State monad for the core controller actor.
type CoreCtlActor = StateActorM CoreActorMsg CoreCtlState

-- }}}

-- {{{ Other Operations

-- | Gets the user list from the database.
-- TODO: Implement database stuff. For now this just returns a hard coded list
-- for testing.
getUserList :: CoreCtlActor [UserId]
getUserList = return [ "Forkk" ]


getConnListeners :: CoreCtlActor [ConnListener]
getConnListeners = return
    [ jsonConnListener $ PortNumber 1337
    , quasselConnListener $ PortNumber 4242
    ]


-- | Starts a user controller for the given IRCUser.
addUserController :: UserId -> CoreCtlActor ()
addUserController userId = do
    me <- lift self
    -- Spawn the user controller and link to it.
    hand <- liftIO $ startUserCtl userId me
    -- lift $ linkActorHandle hand -- TODO: Implement linking in hactor
    modify $ \s -> do
        s { ccUserCtls = M.insert userId hand $ ccUserCtls s }


-- | Starts the given connection listener.
startConnListener :: ConnListener -> CoreCtlActor ()
startConnListener ct = do
    me <- lift self
    hand <- liftIO $ spawnConnListener me ct
    -- lift $ linkActorHandle hand
    modify $ \s -> do
        s { ccConnListeners = hand : ccConnListeners s }


type ConnListenerHandle = ActorHandle ()

-- | Starts a connection listener for the given core type and returns a handle.
spawnConnListener :: CoreCtlHandle -> ConnListener -> IO (ConnListenerHandle)
spawnConnListener coreCtl cl = do
    hand <- spawnActor $ runConnListener coreCtl cl
    return hand

-- | Actor which runs a connection listener.
runConnListener :: CoreCtlHandle -> ConnListener -> ActorM () ()
runConnListener coreCtl cl =
    lift (listenerFunc cl $$ CL.mapM_ (sendIO coreCtl . CoreNewConnection))

-- }}}

-- {{{ Main functions

-- | Entry point for the core controller actor.
-- Initializes the actor, loads the user list, and starts the user controllers.
initCoreCtlActor :: CoreCtlActor ()
initCoreCtlActor = do
    -- Start user controllers.
    getUserList >>= mapM_ addUserController
    -- Start connection listeners.
    getConnListeners >>= mapM_ startConnListener
    -- Proceed on to the main loop.
    coreController

-- | The core controller actor's main function.
coreController :: CoreCtlActor ()
coreController = do
    stmRcv <- lift receiveSTM
    pcs <- gets ccPendingConns
    val <- liftIO $ atomically $
        (Left  <$> waitAnySTM pcs)
         `orElse`
        (Right <$> stmRcv)
    case val of
         Right msg -> handleMessage msg
         Left conn -> handleCompleteConn conn
    coreController

-- | Handles a received message.
handleMessage :: CoreActorMsg -> CoreCtlActor ()
handleMessage (CoreNewConnection conn) = handlePendingConn conn


-- }}}

-- {{{ Event handlers and commands

data CoreCtlCommand = CCmdPlaceholder
    deriving (Typeable)

-- | Handles new pending connections.
handlePendingConn :: PendingConn -> CoreCtlActor ()
handlePendingConn pc = do
    me <- lift self
    -- Start an Async thread for the connection.
    -- TODO: Clean up these threads if the core controller crashes.
    pcThread <- liftIO $ async $ pc me
    -- Add the async thread to our pending connections list.
    modify $ \s -> do
        s { ccPendingConns = pcThread : ccPendingConns s }


type CompleteConn = (UserId, RemoteClient ())

-- | Handles a completed pending connection.
handleCompleteConn :: (Async CompleteConn, CompleteConn) -> CoreCtlActor ()
handleCompleteConn (async, (user, rc)) = do
    -- Remove the pending thread.
    modify $ \s -> do
        s { ccPendingConns = filter (/=async) $ ccPendingConns s }
    -- TODO: Find the correct user controller to attach to.
    -- For now, we'll just attach it to all of them.
    -- Get a list of the user controllers.
    userCtlM <- M.lookup user <$> gets ccUserCtls
    -- Send the client to the user controller.
    case userCtlM of
         Just userCtl -> lift $ ucSendNewClient userCtl rc
         Nothing -> return ()


-- }}}

-- {{{ Utility functions

-- | Sends the given message to the given user controller.
sendUserCtl :: UserCtlActorMsg -> UserCtlHandle -> CoreCtlActor ()
sendUserCtl msg actor = lift $ send actor msg

-- | Waits for any of the given async operations to finish in the STM monad.
waitAnySTM :: [Async a] -> STM (Async a, a)
waitAnySTM threads =
    foldr orElse retry $ map (\a -> waitSTM a >>= return . (a,)) threads

-- }}}

