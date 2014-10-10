{-# LANGUAGE UndecidableInstances #-}
-- | The user controller contains and manages all of the network controllers
-- and client connections for a user. It is responsible for routing events
-- between these objects.
module ChatCore.UserController
    ( UserCtlHandle
    , UserCtlActorMsg (..)
    , UserCtlCtx (..)
    , startUserCtl
    -- * Send Messages
    , ucSendClientCmd
    , ucSendCoreEvt
    , ucSendNewClient
    ) where

import Control.Concurrent.Actor
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Data.Acid
import Data.Default
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T

import ChatCore.Events
import ChatCore.Protocol
import ChatCore.State
import ChatCore.Types
import ChatCore.Util
import {-# SOURCE #-} ChatCore.CoreController
import ChatCore.NetworkController

-- {{{ State and types

data UserCtlState = UserCtlState
    { _usNetCtls    :: M.Map ChatNetworkName NetCtlHandle -- Network controller handles for the user's networks.
    , _usClients    :: [RemoteClientHandle] -- The clients connected to this user.
    , _usCoreCtl    :: CoreCtlHandle
    }
makeLenses ''UserCtlState

instance Default UserCtlState where
    def = UserCtlState
        { _usNetCtls = M.empty
        , _usClients = []
        , _usCoreCtl = undefined
        }

-- | Data structure holding the static context information used by the user
-- controller. This is information that doesn't change, such as user name and
-- acid state handle.
data UserCtlCtx = UserCtlCtx
    { _ucUserName  :: UserName
    , _ucAcidState :: AcidState ChatCoreState
    }
$(makeLenses ''UserCtlCtx)

instance HasAcidState ChatCoreState UserCtlCtx where
    acidStateHandle = view ucAcidState

instance HasChatCoreUser UserCtlCtx where
    currentUserName = view ucUserName


-- | Monad constraint type for the user controller actor.
type UserCtlActor m =
    ( MonadActor UserCtlActorMsg m
    , MonadState UserCtlState m
    , MonadReader UserCtlCtx m
    , MonadBaseControl IO m
    , MonadLogger m
    )

-- }}}

-- {{{ External Interface

data UserCtlActorMsg
    = UserCtlCoreEvent CoreEvent
    | UserCtlClientCommand ClientCommand
    | UserCtlNewClient (RemoteClient ())
instance ActorMessage UserCtlActorMsg

type UserCtlHandle = ActorHandle UserCtlActorMsg


-- | Starts a user controller for the user ID in the database.
startUserCtl :: (MonadIO m, MonadCCState m) =>
    UserName -> CoreCtlHandle -> m UserCtlHandle
startUserCtl uName coreHandle = do
    acid <- getAcidState
    let context = UserCtlCtx uName acid
    -- TODO: Look up the user in the database and load their information.
    liftIO $
        spawnActor $
        runStderrLoggingT $
        (flip runReaderT) context $
        evalStateT initUserCtlActor $ (usCoreCtl .~ coreHandle) def


-- | Sends the given client command to the given user controller.
ucSendClientCmd :: (MonadIO m) => UserCtlHandle -> ClientCommand -> m ()
ucSendClientCmd uctl cmd = send uctl $ UserCtlClientCommand cmd

-- | Sends the given core event to the given user controller.
ucSendCoreEvt :: (MonadIO m) => UserCtlHandle -> CoreEvent -> m ()
ucSendCoreEvt uctl evt = send uctl $ UserCtlCoreEvent evt

-- | Sends the given new client to the given user controller.
ucSendNewClient :: (MonadIO m) => UserCtlHandle -> RemoteClient () -> m ()
ucSendNewClient uctl rc = send uctl $ UserCtlNewClient rc

-- }}}

-- {{{ Other Operations

-- | Starts a network controller for the given IRCNetwork.
addNetController :: (UserCtlActor m) => ChatCoreNetwork -> m ()
addNetController net = do
    me <- self
    -- Spawn the network controller and link to it.
    hand <- startNetCtl (net ^. networkName) me
    -- linkActorHandle hand -- TODO: Implement linking in hactor
    usNetCtls %= M.insert (net ^. networkName) hand

-- | Forwards the given message to the network controller with the given ID.
-- If there is no such network, this function does nothing.
msgToNetwork :: (UserCtlActor m) => ChatNetworkName -> NetCtlActorMsg -> m ()
msgToNetwork netId msg = dropMaybeT $ do
    net <- MaybeT $ use (usNetCtls . at netId)
    send net msg

-- | Forwards the client command to the network controller with the given ID.
-- If there is no such network, this function does nothing.
forwardClientCmd :: (UserCtlActor m) => ChatNetworkName -> ClientCommand -> m ()
forwardClientCmd cnId ccmd = msgToNetwork cnId $ NetCtlClientCmd ccmd

-- }}}

-- {{{ Main functions

-- | Entry point for the user control actor.
-- Initializes the actor, loads the network list, and starts the network
-- controllers.
initUserCtlActor :: (UserCtlActor m) => m ()
initUserCtlActor = do
    -- Start network controllers.
    getUserNetworks >>= mapM_ addNetController
    -- Proceed on to the main loop.
    userController

-- | The user controller actor's main function.
userController :: (UserCtlActor m) => m ()
userController = do
    -- Receive the next message.
    msg <- receive
    case msg of
         UserCtlClientCommand ccmd -> do
             $(logDebugS) "UserController"
                 ("Got client command: " <> (T.pack $ show ccmd))
             handleClientCommand ccmd
         UserCtlCoreEvent evt -> do
             $(logDebugS) "UserController"
                 ("Got core event: " <> (T.pack $ show evt))
             handleCoreEvent evt
         UserCtlNewClient rc -> handleNewClient rc
    userController

-- }}}

-- {{{ Event handlers

-- | Handles client commands.
handleClientCommand :: (UserCtlActor m) => ClientCommand -> m ()

-- Network controller commands.
handleClientCommand msg@(SendMessage netId _ _ _) = forwardClientCmd netId msg
handleClientCommand msg@(JoinChannel netId _)     = forwardClientCmd netId msg
handleClientCommand msg@(PartChannel netId _ _)   = forwardClientCmd netId msg


-- | Handles core events from the network controller.
handleCoreEvent :: (UserCtlActor m) => CoreEvent -> m ()
handleCoreEvent msg = do
    clients <- use usClients
    forM_ clients $ \c -> liftIO $ rcSendCoreEvt c msg


-- | Handles a new client connection.
handleNewClient :: (UserCtlActor m) => RemoteClient () -> m ()
handleNewClient rc = do
    $(logDebugS) "UserController" "New client connected to user controller."
    me <- self
    let clientActor = execRemoteClient me rc
    -- Start an actor for the client.
    rcHandle <- liftIO $ spawnActor clientActor
    -- Add the client's handle to our client list.
    usClients %= (rcHandle :)

-- }}}

