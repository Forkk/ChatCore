module ChatCore.Types where

import Control.Applicative
import Control.Lens (makeClassy)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T

type ChatNetworkName = T.Text

type ChatBufferName = T.Text


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
type ChatUserName = T.Text
type Password = B.ByteString


-- | Represents the status of a network connection.
data ConnectionStatus = Connected | Connecting | Disconnected
                        deriving (Eq, Show, Read)

instance FromJSON ConnectionStatus where
    parseJSON (String "connected") = return Connected
    parseJSON (String "connecting") = return Connecting
    parseJSON (String "disconnected") = return Disconnected
    parseJSON _ = empty

instance ToJSON ConnectionStatus where
    toJSON (Connected) = String "connected"
    toJSON (Connecting) = String "connecting"
    toJSON (Disconnected) = String "disconnected"


-- | Basic information about a chat network.
data ChatNetworkInfo = ChatNetworkInfo
    { _networkName :: ChatNetworkName
    , _networkStatus :: ConnectionStatus
    , _networkUserNick :: Nick
    } deriving (Show)
$(makeClassy ''ChatNetworkInfo)

instance FromJSON ChatNetworkInfo where
    parseJSON (Object obj) = ChatNetworkInfo
                             <$> obj .: "name"
                             <*> obj .: "status"
                             <*> obj .: "user_nick"
    parseJSON _ = error "Invalid chat network info. Must be an object."

instance ToJSON ChatNetworkInfo where
    toJSON (ChatNetworkInfo name status userNick) = object
        [ "name" .= name
        , "status" .= status
        , "user_nick" .= userNick
        ]


data ChatBufferInfo = ChatBufferInfo
    { _biName :: ChatBufferName
    -- , _biType :: ChatBufferType
    , _biActive :: Bool -- ^ True if the user is in this channel.
    , _biUsers :: [Nick]
    } deriving (Show)
