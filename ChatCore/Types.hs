module ChatCore.Types where

import Control.Concurrent.Actor
import qualified Data.Text as T
import Network

import ChatCore.Util.ActorUtil

type ChatNetworkId = T.Text

-- | Represents a message destination. This can be either a channel or a user.
type ChatDest = T.Text

-- | Represents a message source. This works similarly to Destination. It can be either a channel or a user.
type ChatSource = T.Text

-- | Type representing a user's nick.
type Nick = T.Text

-- | Type representing an IRC channel.
type ChatChan = T.Text


-- | A string identifying a user.
type UserId = T.Text


-- | Data structure which describes an IRC network entry.
data IRCNetwork = IRCNetwork
    { inName        :: ChatNetworkId    -- The IRC network's name.
    , inNicks       :: [Nick]           -- A list of nicks for the user to try. Must not be empty.
    , inChannels    :: [ChatChan]       -- A list of the channels to join on this network.
    , inServers     :: [IRCServer]      -- A list of the network's servers.
    }

-- | Data structure which describes an IRC server entry.
data IRCServer = IRCServer
    { servName      :: T.Text
    , servAddress   :: HostName
    , servPort      :: PortID
    }


data CoreCtlHandle = CoreCtlHandle Address

instance ActorHandle CoreCtlHandle where
    actorAddr (CoreCtlHandle addr) = addr

