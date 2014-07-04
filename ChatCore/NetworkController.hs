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
    { netId         :: ChatNetworkId    -- The ID name of this network.
    , netNick       :: Nick             -- The current user's nick.
    , netChannels   :: [ChatChan]       -- A list of channels the current user is in.
    }

-- | State monad for the chat controller.
type NetworkHandler = StateT NetworkState IRC

-- | Executes a NetworkHandler monad with the given ChatState and returns the modified state.
-- Any return value from the NetworkHandler is discarded.
execNetworkHandler :: NetworkHandler a -> NetworkState -> IO NetworkState
execNetworkHandler handler state = execStateT handler state

-- | Sends the given message to the given destination.
handleSendMessage :: ChatDest -> T.Text -> NetworkHandler ()
handleSendMessage dest msg = lift $ sendPrivMsg dest msg

