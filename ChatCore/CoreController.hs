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
import qualified Data.Text as T
import Data.Typeable

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

data CoreCtlState = CoreCtlState
    { ccUserCtls :: M.Map UserId UserCtlHandle -- User controller handles.
    }

instance Default CoreCtlState where
    def = CoreCtlState
        { ccUserCtls = M.empty
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

-- | Starts a user controller for the given IRCUser.
addUserController :: UserId -> CoreCtlActor
addUserController user = do
    me <- lift self
    -- Spawn the user controller and link to it.
    hand <- lift2 $ startUserCtl user (CoreCtlHandle me)
    lift $ linkActorHandle hand
    modify $ \s -> do
        s { ccUserCtls = M.insert (userCtlId hand) hand $ ccUserCtls s }

-- }}}

-- {{{ Main functions

-- | Entry point for the core controller actor.
-- Initializes the actor, loads the user list, and starts the user controllers.
initCoreCtlActor :: CoreCtlActor
initCoreCtlActor = do
    -- Start network actors.
    getUserList >>= mapM_ addUserController
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
        ]

-- }}}

-- {{{ Event handlers and commands

data CoreCtlCommand = CCmdPlaceholder
    deriving (Typeable)

-- | Handles core commands.
handleCoreCommand :: CoreCtlCommand -> CoreCtlActor

handleCoreCommand CCmdPlaceholder = lift2 $ putStrLn "Boop"

-- }}}

-- {{{ Utility functions

lift2 = lift . lift

-- }}}

