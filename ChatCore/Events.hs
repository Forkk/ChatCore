module ChatCore.Events where

import qualified Data.Text as T

-- TODO: Most of these are just placeholders for now. I'll need to design Chat
-- Core's internal protocol later.

-- | Represents a message destination. This can be either a channel or a user.
type ChatDest = T.Text

-- | Represents a message source. This works similarly to Destination. It can be either a channel or a user.
type ChatSource = T.Text


-- | Data structure representing a Chat Core client event.
--   A client event represents a message or request from a client.
data ClientEvent =
    -- | Send the given message to the given destination.
    SendMessage
        { sendMsgDest       :: ChatDest
        , sendMsgContent    :: T.Text
        }


-- | Data structure representing a Chat Core core event.
--   A core event represents something that happened on the server that needs
--   to be translated into a message that the client can understand.
data CoreEvent =
    -- | Represents a new message from the given source.
    ReceivedMessage
        { recvMsgSource     :: ChatSource
        , recvMsgContent    :: T.Text
        }

