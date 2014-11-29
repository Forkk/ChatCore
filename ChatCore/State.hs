{-# LANGUAGE UndecidableInstances #-}
module ChatCore.State (
    -- * Acid State Things
      withLocalState
    
    -- * State Data Structures
    , ChatCoreState (..)
    , chatCoreUsers
    , initialChatCoreState

    , ChatCoreUser (..)
    , usrStName
    , usrStPassword
    , usrStNetworks

    , ChatCoreNetwork (..)
    , netStName
    , netStNicks
    , netStServers
    , netStBuffers

    , ChatCoreNetServer (..)
    , serverHost
    , serverPort

    , ChatCoreBuffer (..)
    , ccBufferName
    , ccBufferActive
    
    -- * FRP Stuff
    , execAcidUpdates
    , executeAcidQuery

    -- * Core

    -- * User
    , getUsers
    , getUser

    , addUser
    , setUserPassword

    -- * Network
    , getNetworks
    , getNetwork

    , addNetwork
    
    , SetNetworkBuffers (..)
    ) where

import Control.Applicative
import Control.Monad.State
import Crypto.PasswordStore
import Data.Acid
import Data.Acid.Advanced hiding (Event)
import qualified Data.ByteString as B
import qualified Data.IxSet as I
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import FRP.Sodium
import FRP.Sodium.IO

import ChatCore.Types
import ChatCore.State.Internal hiding
    ( getUser, getUsers, addUser
    , setUserPassword
    , getUserNetwork, getUserNetworks, addUserNetwork
    , setNetworkNicks, setNetworkServers
    , setChanBufferActive
    )


-- | Executes `UpdateEvent`s from the given FRP event stream.
execAcidUpdates :: (UpdateEvent evt, EventResult evt ~ ()) =>
                    AcidState (EventState evt)
                 -> Event evt
                 -> Reactive (IO ())
execAcidUpdates acid eUpdate =
    listen eUpdate $ update' acid


-- | An FRP event stream of the results of the given queries.
executeAcidQuery :: (QueryEvent evt) =>
                    AcidState (EventState evt)
                 -> Event evt
                 -> Event (EventResult evt)
executeAcidQuery acid eQuery =
    executeAsyncIO (query' acid <$> eQuery)


--------------------------------------------------------------------------------
-- Users
--------------------------------------------------------------------------------

-- | Gets a list of all users.
getUsers :: (MonadIO m) =>
            AcidState ChatCoreState
         -> m [ChatCoreUser]
getUsers acid = query' acid GetUsers

-- | Gets the given user.
getUser :: (MonadIO m) =>
           AcidState ChatCoreState
        -> UserName -> m (Maybe ChatCoreUser)
getUser acid uName = query' acid $ GetUser uName


-- | Adds a user with the given user name and password.
-- The password will be hashed automatically.
addUser :: (MonadIO m) =>
           AcidState ChatCoreState
        -> UserName -> T.Text -> m ()
addUser acid uName password = do
    passHash <- hashUserPassword password
    update' acid $ AddUser uName passHash

-- | Sets the user's password to the given password.
-- This function also handles hashing the password.
setUserPassword :: (MonadIO m) =>
                   AcidState ChatCoreState
                -> UserName -> T.Text -> m ()
setUserPassword acid uName password = do
    passHash <- hashUserPassword password
    update' acid $ SetUserPassword uName passHash


hashUserPassword :: (MonadIO m) => T.Text -> m B.ByteString
hashUserPassword password = liftIO $ makePassword (T.encodeUtf8 password) 16


--------------------------------------------------------------------------------
-- Networks
--------------------------------------------------------------------------------

-- | Gets the the current networks in a `MonadNetworkState` context.
getNetwork :: (MonadIO m) =>
              AcidState ChatCoreState
           -> ChatUserName -> ChatNetworkName
           -> m (Maybe ChatCoreNetwork)
getNetwork acid uName netName =
    query' acid $ GetUserNetwork uName netName

-- | Gets a list of the user's networks.
getNetworks :: (MonadIO m) =>
               AcidState ChatCoreState
            -> ChatUserName
            -> m [ChatCoreNetwork]
getNetworks acid uName =
    query' acid $ GetUserNetworks uName


addNetwork :: (MonadIO m) =>
              AcidState ChatCoreState
           -> ChatUserName
           -> ChatNetworkName
           -> [Nick] -> [ChatCoreNetServer] -> m ()
addNetwork acid uName netName nicks servs =
    update' acid $ AddUserNetwork uName ChatCoreNetwork
                { _netStName = netName
                , _netStNicks = nicks
                , _netStServers = servs
                , _netStBuffers = I.empty
                }
