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
    
    -- * Behavior Stuff
    , holdAcidState
    , linkAcidState

    -- * Core

    -- * User
    , getUsers
    , getUser

    , addUser
    , setUserPassword

    -- * Network
    , getUserNetworks
    , getNetwork

    , addUserNetwork
    
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

import ChatCore.Types
import ChatCore.State.Internal hiding
    ( getUser, getUsers, addUser
    , setUserPassword
    , getUserNetwork, getUserNetworks, addUserNetwork
    , setNetworkNicks, setNetworkServers
    , setChanBufferActive
    )


-- | Creates a new behavior based on the given acid state events.
-- This works similarly to `hold`, taking new values from the given input
-- stream. The initial value will be supplied by the given "getter" event, and
-- the given "setter" event will be used to update the values in the database.
-- The function returns a tuple with the resulting behavior and a function that
-- should be called to clean up.
holdAcidState :: ( QueryEvent getEvt, MethodState getEvt ~ MethodState setEvt
                 , UpdateEvent setEvt, MethodResult setEvt ~ () )
              => AcidState (EventState getEvt)
              -> getEvt
              -> (EventResult getEvt -> setEvt)
              -> Event (EventResult getEvt)
              -> IO (Behavior (EventResult getEvt), IO ())
holdAcidState acid getEvt setEvt event = do
    initVal <- query' acid getEvt
    sync $ do
        bState <- hold initVal event
        cleanup <- listen (value bState) (update' acid . setEvt)
        return (bState, cleanup)

-- | Links the given behavior to the given acid state event.
-- This means the event function will be called each time the behavior changes.
-- This function returns a cleanup IO action.
linkAcidState :: ( UpdateEvent event
                 , EventResult event ~ () )
              => AcidState (EventState event)
              -> (input -> event)
              -> Behavior input
              -> Reactive (IO ())
linkAcidState acid updateFunc behavior =
    listen (updates behavior) (update' acid . updateFunc)

-- {{{ User State Behaviors

-- TODO: Maybe automate some of this with Template Haskell.

-- }}}

-- {{{ Network State Behaviors


-- }}}

-- {{{ Functions

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
    update' acid $ SetUserPassword passHash uName


hashUserPassword :: (MonadIO m) => T.Text -> m B.ByteString
hashUserPassword password = liftIO $ makePassword (T.encodeUtf8 password) 16

-- }}}

-- }}}

-- {{{ `ChatCoreNetwork` Actions

-- | Gets the the current networks in a `MonadNetworkState` context.
getNetwork :: (MonadIO m) =>
              AcidState ChatCoreState
           -> ChatUserName -> ChatNetworkName
           -> m (Maybe ChatCoreNetwork)
getNetwork acid uName netName =
    query' acid $ GetUserNetwork uName netName

-- | Gets a list of the user's networks.
getUserNetworks :: (MonadIO m) =>
                   AcidState ChatCoreState
                -> ChatUserName
                -> m [ChatCoreNetwork]
getUserNetworks acid uName =
    query' acid $ GetUserNetworks uName


addUserNetwork :: (MonadIO m) =>
                  AcidState ChatCoreState
               -> ChatUserName
               -> ChatNetworkName
               -> [Nick] -> [ChatCoreNetServer] -> m ()
addUserNetwork acid uName netName nicks servs =
    update' acid $ AddUserNetwork ChatCoreNetwork
                { _netStName = netName
                , _netStNicks = nicks
                , _netStServers = servs
                , _netStBuffers = I.empty
                } uName

-- }}}
