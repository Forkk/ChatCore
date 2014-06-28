module ChatCore.Types where

import qualified Data.Text as T

-- | Represents a message destination. This can be either a channel or a user.
data ChatDest = DestUser Nick | DestChan ChatChan

-- | Represents a message source. This works similarly to Destination. It can be either a channel or a user.
type ChatSource = T.Text

-- | Type representing a user's nick.
type Nick = T.Text

-- | Type representing an IRC channel.
type ChatChan = T.Text

