{-# LANGUAGE UndecidableInstances #-}
module ChatCore.State.Internal where

import Control.Applicative
import Control.Exception.Lifted
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Lens hiding (Indexable)
import Data.Acid
import Data.Acid.Advanced (query', update')
import qualified Data.ByteString as B
import Data.IxSet
import qualified Data.IxSet as I
import Data.Maybe
import Data.SafeCopy
import qualified Data.Text as T
import Data.Typeable
import Data.Word

import ChatCore.Types

-- | Monad class for things with @AcidState@ handles.
class (Functor m, Applicative m, Monad m, MonadIO m) => MonadAcidState s m where
    getAcidState :: m (AcidState s)

class HasAcidState s a where
    acidStateHandle :: a -> AcidState s

instance HasAcidState s (AcidState s) where
    acidStateHandle = id

instance (Applicative m, MonadIO m, HasAcidState s a, MonadReader a m) =>
         MonadAcidState s m where
    getAcidState = asks acidStateHandle

-- {{{ State Data

-- | Holds information about a server within an IRC netSt.
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
        { _ccBufferName :: ChatBufferName
        , _ccBufferActive :: Bool -- ^ Whether or not to join this channel at startup.
        }
    -- Other type of buffer.
    | ChatCoreOtherBuffer
        { _ccBufferName :: ChatBufferName }
    deriving (Eq, Ord, Show, Read, Typeable)
$(deriveSafeCopy 0 'base ''ChatCoreBuffer)
$(makeLenses ''ChatCoreBuffer)

instance Indexable ChatCoreBuffer where
    empty = ixSet [ ixFun ((:[]) . _ccBufferName) ]


-- | Holds information about an IRC netSt in Chat Core.
data ChatCoreNetwork = ChatCoreNetwork
    { _netStName :: ChatNetworkName -- ^ Unique ID for the netSt.
    , _netStNicks :: [Nick] -- ^ Nicknames to use on this netSt.
    , _netStServers :: [ChatCoreNetServer] -- ^ List of this netSt's servers.
    , _netStBuffers :: IxSet ChatCoreBuffer -- ^ List of buffers in this netSt.
    }
    deriving (Eq, Ord, Show, Read, Typeable)
$(deriveSafeCopy 0 'base ''ChatCoreNetwork)
$(makeLenses ''ChatCoreNetwork)

instance Indexable ChatCoreNetwork where
    empty = ixSet [ ixFun ((:[]) . _netStName) ]


-- | Holds information about a Chat Core user.
data ChatCoreUser = ChatCoreUser
    { _usrStName :: T.Text
    , _usrStPassword :: B.ByteString
    , _usrStNetworks :: IxSet ChatCoreNetwork
    }
    deriving (Eq, Ord, Show, Read, Typeable)
$(deriveSafeCopy 0 'base ''ChatCoreUser)
$(makeLenses ''ChatCoreUser)

instance Indexable ChatCoreUser where
    empty = ixSet [ ixFun ((:[]) . _usrStName) ]


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

-- }}}

-- {{{ State Access Boilerplate

queryM :: forall event m.
    ( Functor m, MonadIO m
    , QueryEvent event, MonadAcidState (EventState event) m) =>
    event -> m (EventResult event)
queryM event = do
    acid <- getAcidState
    query' (acid :: AcidState (EventState event)) event


updateM :: forall event m.
    ( Functor m, MonadIO m
    , UpdateEvent event, MonadAcidState (EventState event) m
    ) =>
    event -> m (EventResult event)
updateM event = do
    acid <- getAcidState
    update' (acid :: AcidState (EventState event)) event


withLocalState :: (IsAcidic s, Typeable s) =>
                  s -- ^ Initial state
               -> (AcidState s -> IO a) -- ^ Function which uses the handle.
               -> IO a
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
getUser :: UserName -> Query ChatCoreState (Maybe ChatCoreUser)
getUser uName = do
    s <- ask
    return $ getOne $ (s ^. chatCoreUsers) @= uName


-- | Adds a new user with the given username and password hash.
addUser :: UserName -> B.ByteString -> Update ChatCoreState ()
addUser uName passHash =
    chatCoreUsers %= insert (ChatCoreUser uName passHash I.empty)

-- | Adds the given netSt to the given user.
-- If a netSt with the same ID exists, it will be replaced.
addUserNetwork :: UserName -> ChatCoreNetwork -> Update ChatCoreState ()
addUserNetwork uName net = updateUserEvt uName (usrStNetworks %~ insert net)

-- | Sets the user's password to the given password hash.
setUserPassword :: UserName -> B.ByteString -> Update ChatCoreState ()
setUserPassword uName passHash = updateUserEvt uName (usrStPassword .~ passHash)

-- {{{ Utility Functions

-- | Apply a function to the user with the given name.
updateUserEvt :: UserName -> (ChatCoreUser -> ChatCoreUser) -> Update ChatCoreState ()
updateUserEvt uName func =
    chatCoreUsers %= \users ->
        let user = fromJust $ getOne (users @= uName)
         in updateIx uName (func user) users

-- }}}

-- }}}

-- {{{ User Network Events

-- | Gets a list of all the given user's netSts.
getUserNetworks :: ChatUserName -> Query ChatCoreState [ChatCoreNetwork]
getUserNetworks uName = do
    userM <- getUser uName
    case userM of
         Just user -> return $ toList (user ^. usrStNetworks)
         -- Should never happen.
         Nothing -> error "Tried to get netSt list for nonexistant user."

-- | Gets the netSt with the given netSt name and user name.
getUserNetwork :: ChatUserName -> ChatNetworkName -> Query ChatCoreState (Maybe ChatCoreNetwork)
getUserNetwork uName netName = runMaybeT $ do
    user <- MaybeT $ getUser uName
    MaybeT $ return $ getOne $ (user ^. usrStNetworks) @= netName


-- | Sets the possible nicks to use on the given netSt.
setNetworkNicks :: ChatUserName -> ChatNetworkName
                -> [Nick] -> Update ChatCoreState ()
setNetworkNicks uName netName nicks =
    updateNetworkEvt uName netName (netStNicks .~ nicks)

-- | Sets the list of servers to connect to on the given netSt.
setNetworkServers :: ChatUserName -> ChatNetworkName
                  -> [ChatCoreNetServer] -> Update ChatCoreState ()
setNetworkServers uName netName servers =
    updateNetworkEvt uName netName (netStServers .~ servers)

setNetworkBuffers :: ChatUserName -> ChatNetworkName 
                  -> IxSet ChatCoreBuffer
                  -> Update ChatCoreState ()
setNetworkBuffers uName netName buffers =
    updateNetworkEvt uName netName (netStBuffers .~ buffers)

-- | Sets the given channel buffer's active flag to the given value.
-- Ignored if the given buffer name is not a channel buffer.
setChanBufferActive :: ChatUserName -> ChatNetworkName -> ChatBufferName
                    -> Bool -> Update ChatCoreState ()
setChanBufferActive uName netName bufName active =
    updateBufferEvt uName netName bufName setActive
  where
    setActive (ChatCoreChannelBuffer n _) =
        ChatCoreChannelBuffer n active
    setActive buf = buf


-- {{{ Utility Functions

-- | Apply a function to the netSt with the given name.
updateNetworkEvt :: ChatUserName -> ChatNetworkName
                 -> (ChatCoreNetwork -> ChatCoreNetwork) -> Update ChatCoreState ()
updateNetworkEvt uName netName func = updateUserEvt uName $
    usrStNetworks %~ \nets ->
        let net = fromJust $ getOne (nets @= netName)
         in updateIx netName (func net) nets

-- | Apply a function to the buffer with the given name.
updateBufferEvt :: ChatUserName -> ChatNetworkName -> ChatBufferName
                -> (ChatCoreBuffer -> ChatCoreBuffer) -> Update ChatCoreState ()
updateBufferEvt uName netName bufName func = updateNetworkEvt uName netName $
    netStBuffers %~ \bufs ->
        let buf = fromJust $ getOne (bufs @= bufName)
         in updateIx bufName (func buf) bufs

-- }}}

-- }}}

$(makeAcidic ''ChatCoreState
    [ 'getUsers
    , 'getUser
    , 'addUser
    , 'addUserNetwork
    , 'setUserPassword

    , 'getUserNetworks
    , 'getUserNetwork
    , 'setNetworkNicks
    , 'setNetworkServers
    , 'setNetworkBuffers
    , 'setChanBufferActive
    ])

-- }}}

