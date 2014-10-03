module ChatCore.Types where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T

type ChatNetworkName = T.Text

type BufferName = T.Text


-- | Represents a message destination. This can be either a channel or a user.
type ChatDest = T.Text

-- | Represents a message source. This works similarly to Destination. It can be either a channel or a user.
type ChatSource = T.Text

-- | Type representing a user's nick.
type Nick = T.Text

-- | Type representing an IRC channel.
type ChatChan = T.Text


-- | A string identifying a user.
type UserName = T.Text


-- | Identifies the type of a message (i.e. PRIVMSG, NOTICE, etc.)
data MessageType = MtPrivmsg | MtNotice deriving (Show, Read, Eq)

instance FromJSON MessageType where
    parseJSON (String "PRIVMSG") = return MtPrivmsg
    parseJSON (String "NOTICE") = return MtNotice
    parseJSON _ = empty

instance ToJSON MessageType where
    toJSON (MtPrivmsg) = String "PRIVMSG"
    toJSON (MtNotice) = String "NOTICE"

