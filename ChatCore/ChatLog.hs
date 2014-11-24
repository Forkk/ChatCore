-- | Main module for the chat logging system.
-- This system is responsible for storing IRC log history. IRC logs are stored
-- in a main logs folder, with sub folders in a <user>/<network>/<buffer> path
-- for each buffer. Each buffer folder contains the log files for that buffer.
-- Log file names follow the format "<dd-mm-yyy>.log", where the date indicates
-- the day that log file is for. Every day should have an individual log file.
module ChatCore.ChatLog
    (
    -- * Buffer Log Stuff
      BufferLogId (..)
    , HasBufferLogId (..)

    , BufferLog
    , mkBufferLog

    -- * Reading and Writing
    , writeBufferLog
    , readBufferLog

    -- * Lines
    , module ChatCore.ChatLog.Line

    -- * Other
    , defLogPath
    ) where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Monoid
import qualified Data.ByteString as B
import Data.Serialize
import qualified Data.Text as T
import Data.Time
import System.Directory
import System.FilePath
import System.IO.Unsafe

import ChatCore.ChatLog.File
import ChatCore.ChatLog.Line
import ChatCore.Types


-- | Hardcoded log path.
-- TODO: Don't hard-code this.
defLogPath :: FilePath
defLogPath = "log"


-- | Data structure representing the path to a specific log file.
data BufferLogId = BufferLogId
    { _blUser :: ChatUserName
    , _blNetwork :: ChatNetworkName
    , _blBuffer :: ChatBufferName
    } deriving (Show, Read, Eq)
$(makeClassy ''BufferLogId)

-- | Represents a loaded buffer log.
-- Currently this contains only a log ID, but it may be used for caching later.
data BufferLog = BufferLog
    { _logRootPath :: FilePath
    , _bufLogId :: BufferLogId
    } deriving (Show, Read)
$(makeLenses ''BufferLog)

instance HasBufferLogId BufferLog where
    bufferLogId = bufLogId


-- | Opens a buffer log in the given directory with the given info.
-- Nonexisting directories will be created when writing to the buffer.
mkBufferLog :: FilePath -> BufferLogId -> BufferLog
mkBufferLog = BufferLog


-- | Gets the path to the buffer log for the given ID.
bufLogIdPath :: (HasBufferLogId l) => l -> FilePath
bufLogIdPath logId = uName </> netName </> bufName
  where
    uName = T.unpack $ view blUser logId
    netName = T.unpack $ view blNetwork logId
    bufName = T.unpack $ view blBuffer logId

-- | Gets the path to the buffer log for the given ID.
bufLogPath :: BufferLog -> FilePath
bufLogPath bufLog = bufLogIdPath bufLog </> view logRootPath bufLog


--------------------------------------------------------------------------------
-- Writing Logs
--------------------------------------------------------------------------------

-- | Writes the given log line to the given buffer log.
writeBufferLog :: BufferLog -> ChatLogLine -> IO ()
writeBufferLog bufLog line = do
    createDirectoryIfMissing True logDir
    B.appendFile logFile $ runPut $ putLogLine line
  where
    logDir = bufLogPath bufLog
    logFile = logDir </> logFileName (logFileForLine line)


--------------------------------------------------------------------------------
-- Reading Logs
--------------------------------------------------------------------------------

-- | Reads a list of lines from the buffer log starting at the given time.
readBufferLog :: BufferLog -> UTCTime -> IO [ChatLogLine]
readBufferLog bufLog startTime = do
    -- Get a list of log files in the given buffer and sort them by date.
    logFiles <- sort <$> filter fileBeforeStart <$> logFilesInDir logDir
    print logFiles
    -- Now read all the files one by one and concatenate them into one big list
    -- of log lines. This is done lazily, so we'll only be reading files we
    -- need.
    -- We're using unsafePerformIO here in order to have this done lazily.
    return $ concatMap doRead logFiles
  where
    logDir = bufLogPath bufLog
    startDay = utctDay startTime
    fileBeforeStart fileId = dayForLogFile fileId <= startDay
    lineBeforeStart line = logLineTime line <= startTime
    -- Reads the given log file.
    doRead file = unsafePerformIO $ do
        putStrLn ("Reading " ++ show file)
        -- TODO: Handle exceptions from readLogFile.
        logLines <- filter lineBeforeStart <$> readLogFile logDir file
        seq logLines $ return logLines

-- | Reads the given log file in the given directory.
readLogFile :: FilePath -> LogFileId -> IO [ChatLogLine]
readLogFile path fid = do
    result <- runGet getLogLines <$> B.readFile logPath
    case result of
         (Left errorMsg) -> do
             putStrLn ("Failed to read log file '" <>
                       logPath <>"' because: " <> errorMsg)
             return []
         (Right logLines) -> return logLines
  where
    logPath = path </> logFileName fid

    
-- | Get action that reads a list of log lines.
-- The list will be in reverse order, so the line at the end of the
-- file will be the first in the list.
getLogLines :: Get [ChatLogLine]
getLogLines = do
    -- TODO: Implement a more robust log file format.
    -- Doing so may require implementing a custom serialization
    -- method, rather than relying on SafeCopy.
    line <- getLogLine
    (line:) <$> (getLogLines <|> return [])
