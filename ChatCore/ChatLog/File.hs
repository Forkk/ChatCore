module ChatCore.ChatLog.File
    ( LogFileId (..)

    , logFileForDate
    , dayForLogFile
    , logFileForToday
    , logFileForLine
    , logFileName

    , parseLogFileId

    , logFilesInDir

    , ChatLogLine (..)
    ) where

import Control.Applicative
import Control.Error
import Data.Time
import System.Directory
import System.FilePath
import Text.Parsec
import Text.Printf

import ChatCore.ChatLog.Line


-- | Identifier for log files.
data LogFileId = LogFileId
    { lfDay     :: Int
    , lfMonth   :: Int
    , lfYear    :: Integer
    } deriving (Show, Read, Eq)

instance Ord LogFileId where
    a <= b =
        (lfYear  a <= lfYear  b) ||
        (lfMonth a <= lfMonth b) ||
        (lfDay   a <= lfDay   b)

-- {{{ Date <-> ID conversions

-- | Gets the log file ID for today.
logFileForToday :: IO LogFileId
logFileForToday = logFileForDate <$> getCurrentTime

-- | Gets the log file ID for the given date.
logFileForDate :: UTCTime -> LogFileId
logFileForDate date = LogFileId day month year
  where
    (year, month, day) = toGregorian $ utctDay date

-- | Gets the UTCTime day for the given log file.
dayForLogFile :: LogFileId -> Day
dayForLogFile (LogFileId day month year) = fromGregorian year month day


-- | Gets the log file ID that the given line should be written to.
logFileForLine :: ChatLogLine -> LogFileId
logFileForLine = logFileForDate . logLineTime

logFileName :: LogFileId -> String
logFileName l = printf "%04i-%02i-%02i.log" (lfYear l) (lfMonth l) (lfDay l)

-- | Parses a string to a `LogFileId`
parseLogFileId :: String -> Maybe LogFileId
parseLogFileId idStr = hush $ parse parser "Log File ID" idStr
  where
    parser = do
        year  <- numSize 4
        _ <- char '-'
        month <- numSize 2
        _ <- char '-'
        day   <- numSize 2
        _ <- string ".log"
        return $ LogFileId day month year
    -- Read n many digits and parse them as an integer.
    numSize n = read <$> count n digit

-- }}}

-- {{{ Listing

-- | Gets a list of log files at the given path.
logFilesInDir :: FilePath -> IO [LogFileId]
logFilesInDir dir =
    mapMaybe parseLogFileId <$> map takeFileName <$> getDirectoryContents dir

-- }}}

