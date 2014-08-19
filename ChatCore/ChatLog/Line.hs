module ChatCore.ChatLog.Line
    ( LogLine (..)

    , LogLineType (..)

    , logLineToString
    , parseLogLine

    , BufferId
    , formatTime'
    , parseTime'
    ) where

import Control.Applicative hiding (many)
import Control.Error
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.Text.Buildable
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Text.Parsec
import System.Locale

import ChatCore.Util.Parsec

type BufferId = T.Text

data LogLineType
    = LogMessage
    | LogNotice
    | LogJoin
    | LogPart
    deriving (Show, Read, Eq)

-- | Represents a line in a chat log.
data LogLine = LogLine
    { logLineBuffer :: T.Text
    , logLineTime   :: UTCTime
    , logLineType   :: LogLineType
    , logLineText   :: T.Text
    } deriving (Show)


timeFormat = "%Y-%m-%d %H:%M:%S UTC"

formatTime' = formatTime defaultTimeLocale timeFormat
parseTime' = parseTime defaultTimeLocale timeFormat


-- | Formats a log line as a string.
logLineToString :: LogLine -> TL.Text
logLineToString line = TL.toLazyText $ logLineBuilder line
  where
    logLineBuilder line =
           TL.fromText "["
        -- TODO: May need to make a custom builder for this, unless we can
        -- parse the timestamp in whatever format this generates.
        <> (TL.fromString $ formatTime' $ logLineTime line)
        <> TL.fromText "|"
        <> (TL.fromString $ show $ logLineType line)
        <> TL.fromText "]: "
        <> (TL.fromText $ logLineText line)

parseLogLine :: BufferId -> B.ByteString -> Maybe LogLine
parseLogLine buf str = hush $ parse parser "Chat Log Line" str
  where
    parser = do
        char '['
        timeM <- parseTime' <$> many (noneOf "|")
        char '|'
        ltype <- read <$> many (noneOf "]")
        char ']'
        char ':'
        char ' '
        text <- T.pack <$> many (anyChar)
        case timeM of
             Just time -> return $ LogLine buf time ltype text
             Nothing -> fail "Invalid timestamp string."

