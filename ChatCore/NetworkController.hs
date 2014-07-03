{-# LANGUAGE RankNTypes #-}
-- | This is a module for handling chat sessions. It handles processing events
-- for a single user one one of that user's IRC networks.
module ChatCore.NetworkController where

import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL

import ChatCore.Protocol
import ChatCore.Events
import ChatCore.IRC
import ChatCore.Types

-- | Data structure which stores the state of a chat session on a network.
data NetworkState = NetworkState
    { csNick        :: Nick         -- The current user's nick.
    , csChannels    :: [ChatChan]   -- A list of channels the current user is in.
    }

-- | State monad for the chat controller.
type NetworkHandler = StateT NetworkState IRC

-- | Executes a NetworkHandler monad with the given ChatState and returns the modified state.
-- Any return value from the NetworkHandler is discarded.
execChatHandler :: NetworkHandler a -> NetworkState -> IRC NetworkState
execChatHandler handler state = execStateT handler state

-- | A consumer which handles a stream of events.
eventHandler :: CoreProtocol conn => NetworkState -> Consumer (ClientEvent, conn) IO NetworkState
eventHandler = CL.foldM $ handler
  where
    handler state evt = execChatHandler (uncurry handleClientEvent $ evt) state

-- | This function handles events from a client connected to the core.
handleClientEvent :: CoreProtocol conn => ClientEvent -> conn -> NetworkHandler ()
handleClientEvent (SendMessage _ dest msg) _ = lift $ sendPrivMsg dest msg

