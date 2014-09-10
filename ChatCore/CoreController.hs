-- | The core controller is ChatCore's main actor. It keeps track of the user
-- controllers and the protocol controller.
module ChatCore.CoreController
    ( CoreCtlHandle
    , CoreActorMsg (..)
    , runCoreCtl
    ) where

import Control.Applicative
import Control.Concurrent.Actor
import Control.Monad
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
import ChatCore.Util.StateActor


-- {{{ External interface

data CoreActorMsg
    = forall conn. CoreProtocol conn => CoreNewConnection conn

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
    }

instance Default CoreCtlState where
    def = CoreCtlState
        { ccUserCtls = M.empty
        , ccConnListeners = []
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


-- Wrapper type so we can store a list of CoreType objects.
data CoreTypeDef = forall ct conn. CoreType ct conn => CoreTypeDef ct

getCoreTypeList :: CoreCtlActor [CoreTypeDef]
getCoreTypeList = return
    [ CoreTypeDef $ jsonCoreType
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


-- | Starts a connection listener for the given core type.
addConnListener :: CoreType ct conn => ct -> CoreCtlActor ()
addConnListener ct = do
    me <- lift self
    hand <- liftIO $ spawnConnListener me ct
    -- lift $ linkActorHandle hand
    modify $ \s -> do
        s { ccConnListeners = hand : ccConnListeners s }


-- | Starts a connection listener for the given core type and returns a handle.
spawnConnListener :: CoreType ct conn =>
                     CoreCtlHandle ->ct -> IO (ConnListenerHandle)
spawnConnListener coreCtl ct = do
    hand <- spawnActor $ runConnListener coreCtl ct
    return hand

-- }}}

-- {{{ Main functions

-- | Entry point for the core controller actor.
-- Initializes the actor, loads the user list, and starts the user controllers.
initCoreCtlActor :: CoreCtlActor ()
initCoreCtlActor = do
    -- Start user controllers.
    getUserList >>= mapM_ addUserController
    -- Start connection listeners.
    getCoreTypeList >>= mapM_ (\(CoreTypeDef ct) -> addConnListener ct)
    -- Proceed on to the main loop.
    coreController

-- | The core controller actor's main function.
coreController :: CoreCtlActor ()
coreController = do
    msg <- lift receive
    case msg of
         CoreNewConnection conn -> handleNewConnection conn
    coreController


-- }}}

-- {{{ Event handlers and commands

data CoreCtlCommand = CCmdPlaceholder
    deriving (Typeable)

-- | Handles connection listener events.
handleNewConnection :: CoreProtocol conn => conn -> CoreCtlActor ()
handleNewConnection conn = do
    -- TODO: Find the correct user controller to attach to.
    -- For now, we'll just attach it to all of them.
    -- Get a list of the user controllers.
    userCtls <- M.elems <$> gets ccUserCtls
    -- Send the message.
    forM_ userCtls $
        sendUserCtl $ UserCtlNewConnection $ ClientConnection conn


-- }}}

-- {{{ Utility functions

-- | Sends the given message to the given user controller.
sendUserCtl :: UserCtlActorMsg -> UserCtlHandle -> CoreCtlActor ()
sendUserCtl msg actor = lift $ send actor msg

-- }}}

