module ChatCore.ChatLog.Line
    ( LogLine (..)
    , LogEvent (..)

    , BufferId

    , logLineToStr
    , parseLogLine
    ) where

import Control.Applicative hiding (many, (<|>))
import Control.Error
import Data.Aeson
import qualified Data.ByteString as B
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.Text.Buildable
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import System.Locale

import ChatCore.Events
import ChatCore.Types
import ChatCore.Util.Parsec

type BufferId = T.Text

-- | Represents an event in the chat log.
data LogEvent
    = LogBufEvent BufferEvent
    deriving (Show, Eq)

-- | Represents a line in a chat log.
data LogLine = LogLine
    { logLineBuffer :: BufferId
    , logLineTime   :: UTCTime
    , logLineEvent  :: LogEvent
    } deriving (Show)

-- {{{ JSON

instance ToJSON LogEvent where
    toJSON (LogBufEvent evt) = toJSON evt

instance FromJSON LogEvent where
    parseJSON obj = LogBufEvent <$> parseJSON obj

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

-- {{{ To/From String

-- Log lines are stored in the following format:
-- [<timestamp>|<metadata>|<metadata>|<metadata>]: <content>
-- Where the ": <content>" part may be omitted if there is no content string.
-- The metadata list is sensitive to order.

-- {{{ Event Info

-- | Gets log line info (metadata and content) for the given buffer event.
bufEvtInfo :: BufferEvent -> ([T.Text], Maybe T.Text)
bufEvtInfo (ReceivedMessage sender content mtype) =
    (["recvmsg", sender, mtypeStr mtype], Just content)
  where
    mtypeStr MtPrivmsg = "PRIVMSG"
    mtypeStr MtNotice = "NOTICE"

bufEvtInfo (UserJoin user) = (["join", user], Nothing)
bufEvtInfo (UserPart user msgM) = (["part", user], msgM)
bufEvtInfo (UserQuit user msgM) = (["quit", user], msgM)


-- | Reads a log event from the given metadata and content.
logEvtFromInfo :: [T.Text] -> Maybe T.Text -> LogEvent

-- Message
logEvtFromInfo ["recvmsg", sender, mtypeStr] (Just msg) =
    LogBufEvent $ ReceivedMessage sender msg (mtype mtypeStr)
  where
    mtype "PRIVMSG" = MtPrivmsg
    mtype "NOTICE" = MtNotice

-- Join, Part, Quit
logEvtFromInfo ["join", user] Nothing = LogBufEvent $ UserJoin user
logEvtFromInfo ["part", user] msgM = LogBufEvent $ UserPart user msgM
logEvtFromInfo ["quit", user] msgM = LogBufEvent $ UserQuit user msgM

-- }}}

-- {{{ Crazy formatting nonsense

logLineToStr :: LogLine -> TL.Text
logLineToStr (LogLine _ time (LogBufEvent evt)) =
    TL.toLazyText $ logLineBuilder time meta contentM
  where
    (meta, contentM) = bufEvtInfo evt

logLineBuilder :: UTCTime -> [T.Text] -> Maybe T.Text -> TL.Builder
logLineBuilder time meta contentM =
       TL.fromText "["
    <> (TL.fromString $ formatTime' time)
    <> metaBuilder meta
    <> TL.fromText "]"
    <> contentBuilder contentM
  where
    metaBuilder [] = mempty
    metaBuilder (meta:metas) =
           TL.fromText "|"
        <> TL.fromText meta
        <> metaBuilder metas

    contentBuilder (Just content) =
           TL.fromText ": "
        <> TL.fromText content
    contentBuilder Nothing = mempty

-- }}}

-- {{{ Crazy parsing nonsense

parseLogLine :: BufferId -> B.ByteString -> Maybe LogLine
parseLogLine bufId line = fromInfo <$> infoM
  where
    fromInfo (time, meta, content) =
        LogLine bufId time $ logEvtFromInfo meta content

    infoM = hush $ parse parser "Chat Log Line" line

    parser = do
        char '['
        -- Timestamp
        timeM <- parseTime' <$> many (noneOf "|")
        meta <- many metaEntry
        char ']'
        contentM <- (Just <$> contentParser) <|> return Nothing
        if isJust timeM
           then return (fromJust timeM, meta, contentM)
           else fail "Invalid timestamp."

    metaEntry = do
        char '|'
        T.pack <$> many (noneOf "|]")

    contentParser = do
        char ':'
        char ' '
        T.pack <$> many anyChar

-- }}}

-- }}}

timeFormat = "%s"

-- TODO: Come up with a better way of doing timestamps. These functions are a
-- major performance bottleneck.
formatTime' = formatTime defaultTimeLocale timeFormat
parseTime' = parseTime defaultTimeLocale timeFormat

