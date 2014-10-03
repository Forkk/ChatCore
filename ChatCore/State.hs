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

-- Accessing Values

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

-- | Queries a list of users from Acid State.
getUsers :: Query ChatCoreState [ChatCoreUser]
getUsers = I.toList <$> _chatCoreUsers <$> ask

-- | Gets the user with the given username.
getUser :: UserName ->
    Query ChatCoreState (Maybe ChatCoreUser)
getUser uName = do
    s <- ask
    return $ getOne $ (s ^. chatCoreUsers) @= uName

-- | Applies a function to the user with the given username.
modUser :: UserName -> (ChatCoreUser -> ChatCoreUser) ->
    Update ChatCoreState ()
modUser uName func =
    chatCoreUsers %= \users ->
        let user = fromJust $ getOne $ (users @= uName)
         in updateIx uName (func user) users


-- | Gets a list of all the given user's networks.
getUserNetworks :: UserName ->
    Query ChatCoreState [ChatCoreNetwork]
getUserNetworks uName = do
    userM <- getUser uName
    case userM of
         Just user -> return $ toList (user ^. userNetworks)
         -- Should never happen.
         Nothing -> error "Tried to get network list for nonexistant user."

-- | Gets the network with the given network name and user name.
getUserNetwork :: UserName -> ChatNetworkName ->
    Query ChatCoreState (Maybe ChatCoreNetwork)
getUserNetwork uName netName = runMaybeT $ do
    user <- MaybeT $ getUser uName
    MaybeT $ return $ getOne $ (user ^. userNetworks) @= netName


$(makeAcidic ''ChatCoreState
    [ 'getUsers
    , 'getUser

    , 'getUserNetworks
    , 'getUserNetwork
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

-- }}}

