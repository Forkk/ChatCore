module ChatCore.ChatLog.Line
    ( ChatLogLine (..)

    , getLogLine
    , putLogLine
    ) where

import Data.Serialize
import Data.SafeCopy
import Data.Time

import ChatCore.Events
import ChatCore.Types

-- | Represents a line in a chat log.
data ChatLogLine = BufLogLine
    { logLineBuffer :: ChatBufferName
    , logLineTime   :: UTCTime
    , logLineEvent  :: BufferEvent
    } deriving (Show, Read, Eq)
$(deriveSafeCopy 0 'base ''ChatLogLine)

getLogLine :: Get ChatLogLine
getLogLine = safeGet

putLogLine :: ChatLogLine -> Put
putLogLine = safePut
