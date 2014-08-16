module ChatCore.ChatLog.File
    ( LogFileId (..)

    , logFileForDate
    , logFileForToday
    , logFileForLine
    , logFileName

    , parseLogFileId

    , LogLine (..)
    ) where

import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import System.IO
import System.FilePath
import Text.Parsec
import Text.Printf

import ChatCore.ChatLog.Line

-- | Identifier for log files.
data LogFileId = LogFileId
    { lfDay     :: Int
    , lfMonth   :: Int
    , lfYear    :: Integer
    } deriving (Show, Eq, Ord)

-- {{{ Date <-> ID conversions

-- | Gets the log file ID for today.
logFileForToday :: IO LogFileId
logFileForToday = logFileForDate <$> getCurrentTime

-- | Gets the log file ID for the given date.
logFileForDate :: UTCTime -> LogFileId
logFileForDate date = LogFileId day month year
  where
    (year, month, day) = toGregorian $ utctDay date

-- | Gets the log file ID that the given line should be written to.
logFileForLine :: LogLine -> LogFileId
logFileForLine = logFileForDate . logLineTime

logFileName :: LogFileId -> String
logFileName l = printf "%04i-%02i-%02i.log" (lfYear l) (lfMonth l) (lfDay l)

-- | Parses a string to a `LogFileId`
parseLogFileId :: String -> Maybe LogFileId
parseLogFileId idStr = hush $ parse parser "Log File ID" idStr
  where
    parser = do
        year  <- numSize 4
        char '-'
        month <- numSize 2
        char '-'
        day   <- numSize 2
        string ".log"
        return $ LogFileId day month year
    -- Read n many digits and parse them as an integer.
    numSize n = read <$> (count n $ digit)

-- }}}

