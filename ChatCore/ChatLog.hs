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
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import Data.Aeson
import Data.List
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Calendar
import Data.Time.Clock
import System.Directory
import System.FilePath
import System.IO
import System.IO.Unsafe

import ChatCore.ChatLog.Line
import ChatCore.ChatLog.File
import ChatCore.Util
import ChatCore.Util.Error

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

writeLogLine :: ChatLog -> LogLine -> IO ()
writeLogLine log line = do
    ensureExists bufDir
    BL.appendFile logFile (encode line `BL8.append` "\n")
  where
    bufDir = chatLogDir log </> T.unpack (logLineBuffer line)
    logFile = bufDir </> logFileName (logFileForLine line)

ensureExists :: FilePath -> IO ()
ensureExists dir = do
    exists <- doesDirectoryExist dir
    if exists
       then return ()
       else createDirectory dir

-- }}}

-- {{{ Read

-- | Reads a list of lines from the given buffer starting at the given time.
readLog :: ChatLog -> BufferId -> UTCTime -> IO [LogLine]
readLog log bufId startTime = do
    -- Get a list of log files in the given buffer and sort them by date.
    logIds <- sort <$> filter fileBeforeStart <$> logFilesInDir bufDir
    -- Now read all the files one by one and concatenate them into one big list
    -- of log lines. This is done lazily, so we'll only be reading files we
    -- need.
    -- We're using unsafePerformIO here in order to have this done lazily.
    return $ concatMap doRead logIds
  where
    bufDir = chatLogDir log </> T.unpack bufId
    startDay = utctDay startTime
    fileBeforeStart fileId = dayForLogFile fileId <= startDay
    lineBeforeStart line = logLineTime line <= startTime
    -- Reads the given log file.
    doRead file = unsafePerformIO $ do
        putStrLn ("Reading " ++ show file)
        reverse <$> filter lineBeforeStart <$> readLogFile log bufId file

-- | Reads the given log file in the given buffer in the given chat log.
readLogFile :: ChatLog -> BufferId -> LogFileId -> IO [LogLine]
readLogFile log bufId fid =
    map setBufId <$> mapMaybe decode <$> BL8.lines <$> BL.readFile logPath
  where
    logPath = chatLogDir log </> T.unpack bufId </> logFileName fid
    setBufId line = line { logLineBuffer = bufId }

-- }}}

