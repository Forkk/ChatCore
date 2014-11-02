-- | Main module for the chat logging system.
-- This system is responsible for storing IRC log history. IRC logs are stored
-- in a main logs folder, with a sub-folder for each IRC network. Each network
-- folder has a folder for each chat buffer on that network. Each buffer folder
-- contains the log files for that buffer.
-- Log file names follow the format "<dd-mm-yyy>.log", where the date indicates
-- the day that log file is for. Every day should have an individual log file.
module ChatCore.ChatLog
    ( ChatLog
    , mkChatLog
    , loadChatLog
    , writeLogLine
    , readLog
    , readLogFile

    , module ChatCore.ChatLog.Line
    ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString as B
import Data.Serialize
import qualified Data.Text as T
import Data.Time
import System.Directory
import System.FilePath
import System.IO.Unsafe

import ChatCore.Types
import ChatCore.ChatLog.Line
import ChatCore.ChatLog.File

-- | The chat logger object. This stores information about the chat logger.
-- Currently this doesn't do much, but it may be used for caching stuff later.
data ChatLog = ChatLog
    { chatLogDir    :: FilePath
    }


-- {{{ Load / Create

-- | Loads a new chat logger with the given log folder.
-- If the folder does not exist, it will be created automatically.
mkChatLog :: FilePath -> IO ChatLog
mkChatLog clogDir =
    maybe create return =<< loadChatLog clogDir
  where
    create = do
        createDirectory clogDir
        fromJust <$> loadChatLog clogDir

-- | Loads an existing chat logger with the given log folder.
-- If the folder does not exist, returns Nothing.
loadChatLog :: FilePath -> IO (Maybe ChatLog)
loadChatLog clogDir = do
    exists <- doesDirectoryExist clogDir
    if exists
       then return $ Just $ ChatLog clogDir
       else return Nothing

-- }}}

-- {{{ Write

writeLogLine :: ChatLog -> ChatLogLine -> IO ()
writeLogLine chatLog line = do
    ensureExists bufDir
    B.appendFile logFile $ runPut $ putLogLine line
  where
    bufDir = chatLogDir chatLog </> T.unpack (logLineBuffer line)
    logFile = bufDir </> logFileName (logFileForLine line)

ensureExists :: FilePath -> IO ()
ensureExists dir = do
    exists <- doesDirectoryExist dir
    unless exists $ createDirectory dir

-- }}}

-- {{{ Read

-- | Reads a list of lines from the given buffer starting at the given time.
readLog :: ChatLog -> ChatBufferName -> UTCTime -> IO [ChatLogLine]
readLog chatLog bufId startTime = do
    -- Get a list of log files in the given buffer and sort them by date.
    logIds <- sort <$> filter fileBeforeStart <$> logFilesInDir bufDir
    print logIds
    -- Now read all the files one by one and concatenate them into one big list
    -- of log lines. This is done lazily, so we'll only be reading files we
    -- need.
    -- We're using unsafePerformIO here in order to have this done lazily.
    return $ concatMap doRead logIds
  where
    bufDir = chatLogDir chatLog </> T.unpack bufId
    startDay = utctDay startTime
    fileBeforeStart fileId = dayForLogFile fileId <= startDay
    lineBeforeStart line = logLineTime line <= startTime
    -- Reads the given log file.
    doRead file = unsafePerformIO $ do
        putStrLn ("Reading " ++ show file)
        -- TODO: Handle exceptions from readLogFile.
        logLines <- filter lineBeforeStart <$> readLogFile chatLog bufId file
        seq logLines $ return logLines

-- | Reads the given log file in the given buffer in the given chat log.
readLogFile :: ChatLog -> ChatBufferName -> LogFileId -> IO [ChatLogLine]
readLogFile chatLog buf fid = do
    result <- runGet getLogLines <$> B.readFile logPath
    case result of
         (Left errorMsg) -> do
             putStrLn ("Failed to read log file '" <>
                       logPath <>"' because: " <> errorMsg)
             return []
         (Right logLines) -> return logLines
  where
    logPath = chatLogDir chatLog </> T.unpack buf </> logFileName fid

    
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

-- }}}

