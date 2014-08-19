module ChatCore.ChatLog.Line
    ( LogLine (..)
    , LogEvent (..)

    , BufferId
    ) where

import Control.Applicative hiding (many)
import Control.Error
import Data.Aeson
import qualified Data.ByteString as B
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.Text.Buildable
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Text.Parsec
import System.Locale

import ChatCore.Types
import ChatCore.Util.Parsec

type BufferId = T.Text

-- | Represents an event in the chat log.
data LogEvent
    = LogMessage
        { logMsgSender  :: User 
        , logMsgContent :: T.Text
        , logMsgType    :: MessageType
        }
    | LogJoin User
    | LogPart User
    | LogQuit User
    deriving (Show, Eq)

-- | Represents a line in a chat log.
data LogLine = LogLine
    { logLineBuffer :: BufferId
    , logLineTime   :: UTCTime
    , logLineEvent  :: LogEvent
    } deriving (Show)

-- {{{ JSON

-- {{{ JSON to Events

instance FromJSON LogEvent where
    parseJSON (Object obj) = do
        evtType <- obj .: "event"
        case evtType :: T.Text of
             "message" -> LogMessage
                <$> obj .: "sender"
                <*> obj .: "message"
                <*> obj .: "msgtype"
             "join" -> LogJoin <$> obj .: "user"
             "part" -> LogPart <$> obj .: "user"
             "quit" -> LogQuit <$> obj .: "user"

-- }}}

-- {{{ Events to JSON

instance ToJSON LogEvent where
    toJSON evt@(LogMessage {}) = object
        [ "event"       .= ("message" :: T.Text)
        , "sender"      .= logMsgSender evt
        , "message"     .= logMsgContent evt
        , "msgtype"     .= logMsgType evt
        ]
    toJSON evt@(LogJoin user) = object
        [ "event" .= ("join" :: T.Text)
        , "user"  .= user
        ]
    toJSON evt@(LogPart user) = object
        [ "event" .= ("part" :: T.Text)
        , "user"  .= user
        ]
    toJSON evt@(LogQuit user) = object
        [ "event" .= ("quit" :: T.Text)
        , "user"  .= user
        ]

-- }}}

-- {{{ Log Line JSON

instance FromJSON LogLine where
    -- The buffer name is not included in the line. It will be added by the
    -- readLogFile function.
    parseJSON (Object obj) = LogLine ""
        <$> obj .: "time"
        <*> obj .: "event"

instance ToJSON LogLine where
    toJSON line = object
        [ "time"    .= logLineTime line
        , "event"   .= logLineEvent line
        ]

-- }}}

-- }}}

