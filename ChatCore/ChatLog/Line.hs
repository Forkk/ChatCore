module ChatCore.ChatLog.Line
    ( LogLine (..)

    , LogLineType (..)

    , logLineToString
    ) where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.Text.Buildable
import Data.Time.Calendar
import Data.Time.Clock

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
    }

-- | Formats a log line as a string.
logLineToString :: LogLine -> TL.Text
logLineToString line = TL.toLazyText $ logLineBuilder line
  where
    logLineBuilder line =
           TL.fromText "["
        -- TODO: May need to make a custom builder for this, unless we can
        -- parse the timestamp in whatever format this generates.
        <> (build $ logLineTime line)
        <> TL.fromText "|"
        <> (TL.fromString $ show $ logLineType line)
        <> TL.fromText "]: "
        <> (TL.fromText $ logLineText line)

