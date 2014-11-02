-- | Module for ChatCore's event system.
-- ChatCore has several different event types, each with its own purpose.
module ChatCore.Events where

import Data.SafeCopy
import qualified Data.Text as T

import ChatCore.Types
import ChatCore.IRC

-- | A buffer event is an event that occurs in a chat buffer.
-- Every buffer event is logged to that buffer's chat log. These events are
-- generally things like a user 
data BufferEvent
    -- | A status message. These are usually either messages from Chat Core or
    -- the IRC server.
    = StatusMessage
        { statusMsgSender   :: T.Text
        , statusMsgContent  :: T.Text
        }
    -- | A user message. These are messages sent by a user.
    | UserMessage
        { msgSender     :: IRCSource
        , msgContent    :: T.Text
        }
    -- | A notice message.
    | NoticeMessage
        { noticeSender  :: IRCSource
        , noticeContent :: T.Text
        }
    -- | An event for when a user joins a channel.
    | UserJoin IRCUser
    -- | An event for when a user leaves a channel.
    | UserPart IRCUser (Maybe T.Text)
    -- | An event for when a user quits from a channel.
    | UserQuit IRCUser (Maybe T.Text)
    -- | An event for when a user changes their nick.
    | OtherNickChange IRCUser T.Text
    deriving (Show, Read, Eq)
$(deriveSafeCopy 0 'base ''BufferEvent)

-- | A network event is an event that relates to an netire network.
-- These are things such as the user's nick changing, or the user
-- losing connection.
data NetworkEvent
    -- | An event occurring when the user connects to the network.
    = NetConnect
    -- | An event occurring when the user disconnects from the network.
    | NetDisconnect
    -- | An event occurring when the current user's nick changes.
    | MyNickChange IRCUser T.Text
    deriving (Show, Read, Eq)
$(deriveSafeCopy 0 'base ''NetworkEvent)

-- | A core event is an event that the server sends to the client.
-- This includes things such as, connecting or disconnecting from IRC and a
-- buffer event occurring.
data CoreEvent
    -- | An event indicating that a buffer event has occurred.
    = ChatBufferEvent
        { bufEvtNetwork :: ChatNetworkName
        , bufEvtBuffer  :: ChatBufferName
        , bufEvt        :: BufferEvent
        }
    | ChatNetworkEvent
        { netEvtNetwork :: ChatNetworkName
        , netEvt        :: NetworkEvent
        }
    deriving (Show, Read, Eq)
$(deriveSafeCopy 0 'base ''CoreEvent)


-- | Data structure representing a Chat Core client command.
--   A client command represents a message or request from a client.
data ClientCommand
    -- | Send the given message to the given destination.
    = SendMessage
        { sendMsgNetwork    :: ChatNetworkName  -- ID of the IRC network to send the message on.
        , sendMsgDest       :: ChatDest         -- Destination to send the message to.
        , sendMsgContent    :: T.Text           -- Content of the message.
        , sendMsgType       :: MessageType      -- The type of message (PRIVMSG or NOTICE).
        }
    | JoinChannel ChatNetworkName ChatChan
    | PartChannel ChatNetworkName ChatChan (Maybe T.Text)
    deriving (Show)
