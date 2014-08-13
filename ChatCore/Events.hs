-- | Module for ChatCore's event data types.
-- There are two different types of events: client commands and core events.
-- Client commands are generated by the protocol handling system. They are
-- events that come from clients connected to the core.
-- Core events are generated by the chat controller. Most come from IRC events
-- that have been processed, but some can come from the core internally. Core
-- events are sent to clients. Which clients they're sent to depends on the
-- situation. Client commands are sent to the chat controller.
module ChatCore.Events where

import qualified Data.Text as T
import Data.Typeable

import ChatCore.Types

-- TODO: Most of these are just placeholders for now. I'll need to design Chat
-- Core's internal protocol later.


-- | Data structure representing a Chat Core client command.
--   A client command represents a message or request from a client.
data ClientCommand =
    -- | Send the given message to the given destination.
    SendMessage
        { sendMsgNetwork    :: ChatNetworkId    -- ID of the IRC network to send the message on.
        , sendMsgDest       :: ChatDest         -- Destination to send the message to.
        , sendMsgContent    :: T.Text           -- Content of the message.
        } |
    JoinChannel ChatNetworkId ChatChan |
    PartChannel ChatNetworkId ChatChan (Maybe T.Text)
    deriving (Show, Typeable)


-- | Data structure representing a Chat Core core event.
--   A core event represents something that happened on the server that needs
--   to be translated into a message that the client can understand.
data CoreEvent =
    -- | Represents a new message from the given source.
    ReceivedMessage
        { recvMsgNetwork    :: ChatNetworkId    -- The network on which the message was received.
        , recvMsgSource     :: ChatSource       -- The source this message was received on (channel or user PM).
        , recvMsgSender     :: Nick             -- The nick of the user who sent the message.
        , recvMsgContent    :: T.Text           -- The content of the message.
        }
    deriving (Show, Typeable)

