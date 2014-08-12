-- | The core controller is ChatCore's main actor. It keeps track of the user
-- controllers and the protocol controller.
module ChatCore.CoreController
    ( CoreCtlHandle
    , runCoreCtl
    ) where

import Control.Applicative
import Control.Concurrent.Actor
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Typeable
import Network

import ChatCore.Protocol
import ChatCore.Protocol.JSON
import ChatCore.Types
import ChatCore.UserController
import ChatCore.Util.ActorUtil
import ChatCore.Util.StateActor


-- {{{ External interface

-- Handle data type declared in types module to prevent cyclical imports.

-- | Starts a core controller on the current thread.
-- This does **not** fork to another thread.
runCoreCtl :: IO ()
runCoreCtl = runActor $ runStateActor initCoreCtlActor $ def

-- }}}

-- {{{ State and types

-- Handle for connection listener actors.
data ConnListenerHandle = forall ctype conn. CoreType ctype conn =>
    ConnListenerHandle
    { clhAddr :: Address
    , clhType :: ctype
    } deriving (Typeable)

instance ActorHandle ConnListenerHandle where
    actorAddr = clhAddr


data CoreCtlState = CoreCtlState
    { ccUserCtls :: M.Map UserId UserCtlHandle -- User controller handles.
    , ccConnListeners :: [ConnListenerHandle]
    }

instance Default CoreCtlState where
    def = CoreCtlState
        { ccUserCtls = M.empty
        , ccConnListeners = []
        }

-- | State monad for the core controller actor.
type CoreCtlActor = StateActor CoreCtlState
type CoreCtlActorM = StateActorM CoreCtlState

-- }}}

-- {{{ Other Operations

-- | Gets the user list from the database.
-- TODO: Implement database stuff. For now this just returns a hard coded list
-- for testing.
getUserList :: CoreCtlActorM [UserId]
getUserList = return [ "Forkk" ]


-- Wrapper type so we can store a list of CoreType objects.
data CoreTypeDef = forall ct conn. CoreType ct conn => CoreTypeDef ct

getCoreTypeList :: CoreCtlActorM [CoreTypeDef]
getCoreTypeList = return
    [ CoreTypeDef $ JSONCoreType $ PortNumber 1337
    ]


-- | Starts a user controller for the given IRCUser.
addUserController :: UserId -> CoreCtlActor
addUserController user = do
    me <- lift self
    -- Spawn the user controller and link to it.
    hand <- lift2 $ startUserCtl user (CoreCtlHandle me)
    lift $ linkActorHandle hand
    modify $ \s -> do
        s { ccUserCtls = M.insert (userCtlId hand) hand $ ccUserCtls s }


-- | Starts a connection listener for the given core type.
addConnListener :: CoreType ct conn => ct -> CoreCtlActor
addConnListener ct = do
    me <- lift self
    hand <- liftIO $ spawnConnListener me ct
    lift $ linkActorHandle hand
    modify $ \s -> do
        s { ccConnListeners = hand : ccConnListeners s }


-- | Starts a connection listener for the given core type and returns a handle.
spawnConnListener :: CoreType ct conn => Address ->ct -> IO (ConnListenerHandle)
spawnConnListener coreCtl ct = do
    addr <- spawn $ runConnListener coreCtl ct
    return $ ConnListenerHandle addr ct

-- }}}

-- {{{ Main functions

-- | Entry point for the core controller actor.
-- Initializes the actor, loads the user list, and starts the user controllers.
initCoreCtlActor :: CoreCtlActor
initCoreCtlActor = do
    -- Start user controllers.
    getUserList >>= mapM_ addUserController
    -- Start connection listeners.
    getCoreTypeList >>= mapM_ (\(CoreTypeDef ct) -> addConnListener ct)
    -- Proceed on to the main loop.
    coreController

-- | The core controller actor's main function.
coreController :: CoreCtlActor
coreController = do
    -- Get the state.
    state <- get
    -- Make a function to wrap handlers.
    let handler :: forall m. Typeable m => (m -> CoreCtlActor) -> Handler
        handler = stateActorHandler coreController state
    lift $ receive $
        [ handler handleCoreCommand
        , handler handleNewConnEvent
        ]

-- }}}

-- {{{ Event handlers and commands

data CoreCtlCommand = CCmdPlaceholder
    deriving (Typeable)

-- | Handles core commands.
handleCoreCommand :: CoreCtlCommand -> CoreCtlActor

handleCoreCommand CCmdPlaceholder = lift2 $ putStrLn "Boop"


-- | Handles connection listener events.
handleNewConnEvent :: NewConnEvent -> CoreCtlActor
handleNewConnEvent (NewConnection connAction) = do
    -- Run the connection action.
    conn <- ClientConnection <$> liftIO connAction
    -- TODO: Find the correct user controller to send this to.
    -- For now, we'll just attach it to all of them.
    mapM_ (lift . msgActorSend (AddConn conn)) =<< M.elems <$> gets ccUserCtls


-- }}}

-- {{{ Utility functions

lift2 = lift . lift

-- }}}

