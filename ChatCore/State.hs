{-# LANGUAGE UndecidableInstances #-}
module ChatCore.State (
    -- * Acid State Things
      withLocalState
    
    -- * State Data Structures
    , ChatCoreState (..)
    , chatCoreUsers
    , initialChatCoreState

    , ChatCoreUser (..)
    , userName
    , userPassword
    , userNetworks

    , ChatCoreNetwork (..)
    , networkName
    , networkNicks
    , networkServers
    , networkBuffers

    , ChatCoreNetServer (..)
    , serverHost
    , serverPort

    , ChatCoreBuffer (..)
    , ccBufferName
    , ccBufferActive

    -- * Monad Classes
    , MonadAcidState (..)
    , MonadCCState

    , MonadUserState (..)
    , MonadNetworkState (..)

    -- * Context Classes
    , HasAcidState (..)
    , HasChatCoreUser (..)
    , HasChatCoreNetwork (..)

    -- * Core Actions

    -- * User Actions
    , getUsers
    , getUser
    , getUserByName

    , addUser
    , setUserPassword

    -- * Network Actions
    , getUserNetworks
    , getUserNetwork
    , getNetwork

    , addUserNetwork
    ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.Acid
import qualified Data.ByteString as B
import qualified Data.IxSet as I
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import ChatCore.Types
import ChatCore.State.Internal hiding
    ( getUser, getUsers, addUser
    , setUserPassword
    , getUserNetwork, getUserNetworks, addUserNetwork
    , setNetworkNicks, setNetworkServers
    , addNetworkBuffer, delNetworkBuffer
    , setChanBufferActive
    )

type MonadCCState = MonadAcidState ChatCoreState

-- {{{ For `ChatCoreUser`

-- {{{ Classes

-- | Class for data structures with a `UserName` field referring to a specific
-- `ChatCoreUser`. Reader monads with instances of this class as a value will
-- be instances of `MonadReader ChatCoreUser`.
class HasChatCoreUser s where
    currentUserName :: s -> UserName

-- | Monad class for contexts that have access to a user in the database.
-- The user is assumed to exist.
class (MonadCCState m) => MonadUserState m where
    getUserName :: (MonadUserState m) => m UserName

instance (MonadCCState m, HasChatCoreUser s, MonadReader s m) =>
         MonadUserState m where
    getUserName = asks currentUserName

-- }}}

-- {{{ Actions

-- | Gets a list of all users.
getUsers :: (MonadCCState m) => m [ChatCoreUser]
getUsers = queryM $ GetUsers

-- | Gets the given user.
getUserByName :: (MonadCCState m) => UserName -> m (Maybe ChatCoreUser)
getUserByName uName = queryM $ GetUser uName

-- | Gets the current user.
getUser :: (MonadUserState m) => m ChatCoreUser
getUser = do
    uName <- getUserName
    fromJust <$> getUserByName uName


-- | Adds a user with the given user name and password.
-- The password will be hashed automatically.
addUser :: (MonadCCState m) => UserName -> T.Text -> m ()
addUser uName password = do
    passHash <- hashUserPassword password
    updateM $ AddUser uName passHash

-- | Sets the user's password to the given password.
-- This function also handles hashing the password.
setUserPassword :: (MonadUserState m) => T.Text -> m ()
setUserPassword password = do
    passHash <- hashUserPassword password
    updateUser $ SetUserPassword passHash


-- | Runs the given event on the current user.
updateUser ::
    ( MonadUserState m
    , MonadAcidState (EventState event) m
    , UpdateEvent event) =>
    (UserName -> event) -> m (EventResult event)
updateUser evt = do
    uName <- getUserName
    updateM (evt uName)

hashUserPassword :: (MonadIO m) => T.Text -> m B.ByteString
hashUserPassword password = liftIO $ makePassword (T.encodeUtf8 password) 16

-- }}}

-- }}}

-- {{{ For `ChatCoreNetwork`

-- {{{ Classes

-- | Class for data structures with a `ChatNetworkName` field referring to a
-- specific `ChatCoreNetwork`. Reader monads with instances of this class as a
-- value will be instances of `MonadReader ChatCoreNetwork`.
class HasChatCoreNetwork s where
    currentNetworkName :: s -> ChatNetworkName

-- | Monad class for contexts that have access to a network in the database.
-- The network is assumed to exist.
class MonadUserState m => MonadNetworkState m where
    getNetworkName :: (MonadUserState m) => m UserName

instance (HasChatCoreNetwork s, MonadReader s m, MonadUserState m) =>
         MonadNetworkState m where
    getNetworkName = asks currentNetworkName

-- }}}

-- {{{ Actions

-- | Gets the the current network in a `MonadNetworkState` context.
getNetwork :: (MonadNetworkState m) =>
    m ChatCoreNetwork
getNetwork = do
    netName <- getNetworkName
    fromJust <$> getUserNetwork netName

-- | Gets a list of the user's networks.
getUserNetworks :: (MonadUserState m) => m [ChatCoreNetwork]
getUserNetworks = do
    uName <- getUserName
    queryM $ GetUserNetworks uName

-- | Gets the network with the given name from the current user.
getUserNetwork :: (MonadUserState m) =>
    ChatNetworkName -> m (Maybe ChatCoreNetwork)
getUserNetwork netName = do
    uName <- getUserName
    queryM $ GetUserNetwork netName uName


addUserNetwork :: (MonadUserState m) =>
    ChatNetworkName -> [Nick] -> [ChatCoreNetServer] -> m ()
addUserNetwork netName nicks servs = do
    updateUser $ AddUserNetwork $ ChatCoreNetwork
        { _networkName = netName
        , _networkNicks = nicks
        , _networkServers = servs
        , _networkBuffers = I.empty
        }


-- | Runs the given event on the current network.
updateNetwork ::
    ( MonadUserState m, MonadNetworkState m
    , MonadAcidState (EventState event) m
    , UpdateEvent event) =>
    (ChatNetworkName -> UserName -> event) -> m (EventResult event)
updateNetwork evt = do
    uName <- getUserName
    netName <- getNetworkName
    updateM (evt uName netName)

-- }}}

-- }}}

