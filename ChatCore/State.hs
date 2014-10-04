{-# LANGUAGE UndecidableInstances #-}
module ChatCore.State where

import Control.Applicative
import Control.Exception.Lifted
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Lens hiding (Indexable)
import Data.Acid
import Data.Acid.Advanced (query', update')
import Data.IxSet
import qualified Data.IxSet as I
import Data.Maybe
import Data.SafeCopy
import qualified Data.Text as T
import Data.Typeable
import Data.Word

import ChatCore.Types


-- | Holds information about a server within an IRC network.
data ChatCoreNetServer = ChatCoreNetServer
    { _serverHost :: T.Text
    , _serverPort :: Word16
    }
    deriving (Eq, Ord, Show, Read, Typeable)
$(deriveSafeCopy 0 'base ''ChatCoreNetServer)
$(makeLenses ''ChatCoreNetServer)

-- | Holds persistent information about a buffer in Chat Core.
data ChatCoreBuffer
    -- Buffer for an IRC channel.
    = ChatCoreChannelBuffer
        { _ccBufferName :: BufferName
        , _ccBufferActive :: Bool -- ^ Whether or not to join this channel at startup.
        }
    -- Other type of buffer.
    | ChatCoreOtherBuffer
        { _ccBufferName :: BufferName }
    deriving (Eq, Ord, Show, Read, Typeable)
$(deriveSafeCopy 0 'base ''ChatCoreBuffer)
$(makeLenses ''ChatCoreBuffer)

instance Indexable ChatCoreBuffer where
    empty = ixSet [ ixFun ((:[]) . _ccBufferName) ]


-- | Holds information about an IRC network in Chat Core.
data ChatCoreNetwork = ChatCoreNetwork
    { _networkName :: ChatNetworkName -- ^ Unique ID for the network.
    , _networkNicks :: [Nick] -- ^ Nicknames to use on this network.
    , _networkServers :: [ChatCoreNetServer] -- ^ List of this network's servers.
    , _networkBuffers :: IxSet ChatCoreBuffer -- ^ List of buffers in this network.
    }
    deriving (Eq, Ord, Show, Read, Typeable)
$(deriveSafeCopy 0 'base ''ChatCoreNetwork)
$(makeLenses ''ChatCoreNetwork)

instance Indexable ChatCoreNetwork where
    empty = ixSet [ ixFun ((:[]) . _networkName) ]


-- | Holds information about a Chat Core user.
data ChatCoreUser = ChatCoreUser
    { _userName :: T.Text
    , _userNetworks :: IxSet ChatCoreNetwork
    }
    deriving (Eq, Ord, Show, Read, Typeable)
$(deriveSafeCopy 0 'base ''ChatCoreUser)
$(makeLenses ''ChatCoreUser)

instance Indexable ChatCoreUser where
    empty = ixSet [ ixFun ((:[]) . _userName) ]


-- | The Acid State database state.
data ChatCoreState = ChatCoreState
    { _chatCoreUsers :: IxSet ChatCoreUser
    }
    deriving (Eq, Ord, Show, Read, Typeable)
$(deriveSafeCopy 0 'base ''ChatCoreState)
$(makeLenses ''ChatCoreState)


-- | The initial Chat Core state set on first run.
initialChatCoreState :: ChatCoreState
initialChatCoreState = ChatCoreState { _chatCoreUsers = I.empty }


-- | Monad class for things with @AcidState@ handles.
class (Functor m, Applicative m, Monad m, MonadIO m) => HasAcidState m s where
    getAcidState :: m (AcidState s)

-- {{{ State Access Boilerplate

instance (Applicative m, MonadIO m, MonadReader (AcidState s) m) =>
         HasAcidState m s where
    getAcidState = ask


queryM :: forall event m.
    ( Functor m, MonadIO m
    , QueryEvent event, HasAcidState m (EventState event)) =>
    event -> m (EventResult event)
queryM event = do
    acid <- getAcidState
    query' (acid :: AcidState (EventState event)) event


updateM :: forall event m.
    ( Functor m, MonadIO m
    , UpdateEvent event, HasAcidState m (EventState event)
    ) =>
    event -> m (EventResult event)
updateM event = do
    acid <- getAcidState
    update' (acid :: AcidState (EventState event)) event


withLocalState ::
    ( MonadBaseControl IO m, MonadIO m
    , IsAcidic s, Typeable s
    ) =>
    s -- ^ Initial state
 -> (AcidState s -> m a) -- ^ Function which uses the handle.
 -> m a
withLocalState initialState =
    bracket (liftIO $ openLocalState initialState)
            (liftIO . createCheckpointAndClose)
  where
    createCheckpointAndClose acid = do
        createCheckpoint acid
        closeAcidState acid

-- }}}

-- {{{ Acid State Events

-- {{{ User Events

-- | Queries a list of users from Acid State.
getUsers :: Query ChatCoreState [ChatCoreUser]
getUsers = I.toList <$> _chatCoreUsers <$> ask

-- | Gets the user with the given username.
getUser :: UserName ->
    Query ChatCoreState (Maybe ChatCoreUser)
getUser uName = do
    s <- ask
    return $ getOne $ (s ^. chatCoreUsers) @= uName

-- | Adds the given network to the given user.
-- If a network with the same ID exists, it will be replaced.
addUserNetwork :: ChatCoreNetwork -> UserName -> Update ChatCoreState ()
addUserNetwork net = updateUserEvt (userNetworks %~ insert net)

-- {{{ Utility Functions

-- | Apply a function to the user with the given name.
updateUserEvt :: (ChatCoreUser -> ChatCoreUser) -> UserName -> Update ChatCoreState ()
updateUserEvt func uName =
    chatCoreUsers %= \users ->
        let user = fromJust $ getOne $ (users @= uName)
         in updateIx uName (func user) users

-- }}}

-- }}}

-- {{{ User Network Events

-- | Gets a list of all the given user's networks.
getUserNetworks :: UserName -> Query ChatCoreState [ChatCoreNetwork]
getUserNetworks uName = do
    userM <- getUser uName
    case userM of
         Just user -> return $ toList (user ^. userNetworks)
         -- Should never happen.
         Nothing -> error "Tried to get network list for nonexistant user."

-- | Gets the network with the given network name and user name.
getUserNetwork :: UserName -> ChatNetworkName -> Query ChatCoreState (Maybe ChatCoreNetwork)
getUserNetwork uName netName = runMaybeT $ do
    user <- MaybeT $ getUser uName
    MaybeT $ return $ getOne $ (user ^. userNetworks) @= netName


-- | Sets the possible nicks to use on the given network.
setNetworkNicks :: [Nick] -> UserName -> ChatNetworkName -> Update ChatCoreState ()
setNetworkNicks nicks =
    updateNetworkEvt (networkNicks .~ nicks)

-- | Sets the list of servers to connect to on the given network.
setNetworkServers ::
    [ChatCoreNetServer] ->
    UserName -> ChatNetworkName ->
    Update ChatCoreState ()
setNetworkServers servers =
    updateNetworkEvt (networkServers .~ servers)


addNetworkBuffer ::
    ChatCoreBuffer ->
    UserName -> ChatNetworkName ->
    Update ChatCoreState ()
addNetworkBuffer buffer =
    updateNetworkEvt (networkBuffers %~ insert buffer)

delNetworkBuffer ::
    BufferName ->
    UserName -> ChatNetworkName ->
    Update ChatCoreState ()
delNetworkBuffer bufName = do
    updateNetworkEvt (networkBuffers %~ deleteIx bufName)

-- | Sets the given channel buffer's active flag to the given value.
-- Ignored if the given buffer name is not a channel buffer.
setChanBufferActive :: BufferName -> Bool ->
    UserName -> ChatNetworkName -> Update ChatCoreState ()
setChanBufferActive buffer active = do
    updateBufferEvt buffer setActive
  where
    setActive (ChatCoreChannelBuffer n _) =
        ChatCoreChannelBuffer n active
    setActive buf = buf


-- {{{ Utility Functions

-- | Apply a function to the network with the given name.
updateNetworkEvt ::
    (ChatCoreNetwork -> ChatCoreNetwork) ->
    UserName -> ChatNetworkName ->
    Update ChatCoreState ()
updateNetworkEvt func uName netName = (flip updateUserEvt) uName $
    userNetworks %~ \nets ->
        let net = fromJust $ getOne $ (nets @= netName)
         in updateIx netName (func net) nets

-- | Apply a function to the buffer with the given name.
updateBufferEvt ::
    BufferName -> (ChatCoreBuffer -> ChatCoreBuffer) ->
    UserName -> ChatNetworkName ->
    Update ChatCoreState ()
updateBufferEvt bufName func = updateNetworkEvt $
    networkBuffers %~ \bufs ->
        let buf = fromJust $ getOne $ (bufs @= bufName)
         in updateIx bufName (func buf) bufs
-- }}}

-- }}}

$(makeAcidic ''ChatCoreState
    [ 'getUsers
    , 'getUser
    , 'addUserNetwork

    , 'getUserNetworks
    , 'getUserNetwork
    , 'setNetworkNicks
    , 'setNetworkServers
    , 'addNetworkBuffer
    , 'delNetworkBuffer
    , 'setChanBufferActive
    ])

-- }}}

-- {{{ Reader Monads

-- | Monad class for data structures with a `UserName` field referring to a
-- specific `ChatCoreUser`. Reader monads with instances of this class as a
-- value will be instances of `MonadReader ChatCoreUser`.
class HasChatCoreUser s where
    getCurrentUserName :: s -> UserName

viewUser :: (HasChatCoreUser s, MonadState s m, HasAcidState m ChatCoreState) =>
    Lens' ChatCoreUser a -> m a
viewUser l = do
    uName <- gets getCurrentUserName
    user <- fromJust <$> (queryM $ GetUser uName)
    return $ view l $ user

-- | Runs the given event on the current user.
updateUser ::
    ( HasChatCoreUser s, MonadState s m
    , HasAcidState m (EventState event), UpdateEvent event) =>
    (UserName -> event) -> m (EventResult event)
updateUser evt = do
    uName <- gets getCurrentUserName
    updateM (evt uName)


-- | Monad class for contexts which have access to a particular
-- `ChatCoreNetwork`.
class HasChatCoreUser s => HasChatCoreNetwork s where
    getCurrentNetworkName :: s -> ChatNetworkName

-- | Applies a lens to the current network in a `HasChatCoreNetwork` monad.
viewNetwork :: (HasChatCoreNetwork s, MonadState s m, HasAcidState m ChatCoreState) =>
    Lens' ChatCoreNetwork a -> m a
viewNetwork l = do
    uName <- gets getCurrentUserName
    netName <- gets getCurrentNetworkName
    net <- fromJust <$> (queryM $ GetUserNetwork uName netName)
    return $ view l $ net

-- | Runs the given event on the current network.
updateNetwork ::
    ( HasChatCoreUser s, HasChatCoreNetwork s, MonadState s m
    , HasAcidState m (EventState event), UpdateEvent event) =>
    (UserName -> ChatNetworkName -> event) -> m (EventResult event)
updateNetwork evt = do
    uName <- gets getCurrentUserName
    netName <- gets getCurrentNetworkName
    updateM (evt uName netName)

-- }}}

