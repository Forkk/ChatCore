{-# LANGUAGE UndecidableInstances, IncoherentInstances #-}
-- | The user controller contains and manages all of the network controllers
-- and client connections for a user. It is responsible for routing events
-- between these objects.
module ChatCore.UserController
    ( UserCtlHandle
    , UserCtlActorMsg (..)
    , startUserCtl
    -- * Send Messages
    , ucSendClientCmd
    , ucSendCoreEvt
    , ucSendNewClient
    ) where

import Control.Applicative
import Control.Concurrent.Actor
import Control.Lens
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql

import ChatCore.Database
import ChatCore.Events
import ChatCore.Protocol
import ChatCore.Types
import ChatCore.Util
import {-# SOURCE #-} ChatCore.CoreController
import ChatCore.NetworkController

-- {{{ State and types

data UserCtlState = UserCtlState
    { _usNetCtls    :: M.Map ChatNetworkName NetCtlHandle -- Network controller handles for the user's networks.
    , _usClients    :: [RemoteClientHandle] -- The clients connected to this user.
    , _usUserName   :: UserName
    , _usCoreCtl    :: CoreCtlHandle
    , _usConnPool   :: ConnectionPool
    }
makeLenses ''UserCtlState

instance Default UserCtlState where
    def = UserCtlState
        { _usNetCtls = M.empty
        , _usClients = []
        -- The undefined should be OK in this case. If the username is somehow
        -- not set, the actor should crash anyway.
        , _usUserName = undefined
        , _usCoreCtl = undefined
        , _usConnPool = undefined
        }

-- | Monad constraint type for the user controller actor.
type UserCtlActor m =
    ( MonadActor UserCtlActorMsg m
    , MonadState UserCtlState m
    , MonadBaseControl IO m
    , MonadLogger m)

runDB'UC :: (UserCtlActor m) => SqlPersistT m a -> m a
runDB'UC t = do
    pool <- use usConnPool
    runSqlPool t pool

-- }}}

-- {{{ External Interface

data UserCtlActorMsg
    = UserCtlCoreEvent CoreEvent
    | UserCtlClientCommand ClientCommand
    | UserCtlNewClient (RemoteClient ())
instance ActorMessage UserCtlActorMsg

type UserCtlHandle = ActorHandle UserCtlActorMsg


-- | Starts a user controller for the user ID in the database.
startUserCtl :: UserName -> CoreCtlHandle -> ConnectionPool -> IO UserCtlHandle
startUserCtl userName coreHandle pool =
    -- TODO: Look up the user in the database and load their information.
    spawnActor $
    runStderrLoggingT $
    evalStateT initUserCtlActor $
          (usUserName .~ userName)
        $ (usCoreCtl .~ coreHandle)
        $ (usConnPool .~ pool)
        $ def


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

-- | Gets the user's network list from the database.
-- TODO: Implement database stuff. For now this just returns a hard coded list
-- for testing.
getNetworkList :: (UserCtlActor m) => m [IrcNetwork]
getNetworkList = runDB'UC $ do
    $(logDebugS) "UserController" "Getting network list."
    userName <- use usUserName
    $(logDebugS) "UserController" "Done"
    -- Crash if the user is nonexistant.
    user <- fromJust <$> getBy (UniqueUserName userName)
    map entityVal <$> selectList [IrcNetworkUser ==. entityKey user] []

-- | Starts a network controller for the given IRCNetwork.
addNetController :: (UserCtlActor m) => IrcNetwork -> m ()
addNetController net = do
    me <- self
    -- Spawn the network controller and link to it.
    hand <- liftIO $ startNetCtl net me
    -- linkActorHandle hand -- TODO: Implement linking in hactor
    usNetCtls %= M.insert (ircNetworkName net) hand

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
    -- Start network actors.
    getNetworkList >>= mapM_ addNetController
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
                 ("Got client command: " `T.append` (T.pack $ show ccmd))
             handleClientCommand ccmd
         UserCtlCoreEvent evt -> do
             $(logDebugS) "UserController"
                 ("Got core event: " `T.append` (T.pack $ show evt))
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

