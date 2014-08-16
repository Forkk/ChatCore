module ChatCore.ChatLog.Buffer
    ( BufferLog
    , bufferId
    , bufferDir
    , BufferLogId

    , loadBufferLog
    , createBufferLog
    , mkBufferLog

    , bufferLogSink
    ) where

import Control.Applicative
import Control.Error
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import Data.Conduit
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory
import System.FilePath

import ChatCore.ChatLog.File
import ChatCore.ChatLog.Line
import ChatCore.Util.Error

type BufferLogId = T.Text

-- | Represents a log for a single chat buffer.
-- This data type and its related functions are responsible for managing the
-- log files for a specific buffer.
data BufferLog = BufferLog
    { bufferDir     :: FilePath
    , bufferId      :: BufferLogId
    }

-- {{{ Create / Load

-- | Loads the buffer log in the given directory with the given ID.
-- If no such log exists, it will be created.
-- Will cause an exception if creating the buffer log fails.
mkBufferLog :: FilePath -> BufferLogId -> IO (BufferLog)
mkBufferLog logDir bufId = do
    -- Try to load the buffer log.
    bufLogM <- loadBufferLog logDir bufId
    case bufLogM of
         Just bufLog -> return bufLog
         Nothing -> createBufferLog logDir bufId

-- | Creates a new buffer log in the given directory with the given ID.
-- The path to the buffer log's folder will be "<Directory>/<Buffer ID>".
-- Will fail with an exception if there is already a folder with the given
-- buffer log ID in the given directory.
createBufferLog :: FilePath -> BufferLogId -> IO (BufferLog)
createBufferLog logDir bufId = do
    -- TODO: Return Nothing on failure.
    -- Create the buffer directory.
    createDirectory bufDir
    -- Open it.
    fromJust <$> loadBufferLog logDir bufId
  where
    bufDir = logDir </> T.unpack bufId

-- | Loads a buffer log with the given ID in the given directory.
-- If the given log doesn't exist, returns Nothing.
loadBufferLog :: FilePath -> BufferLogId -> IO (Maybe BufferLog)
loadBufferLog logDir bufId = tryMaybe $ do
    -- TODO: Return Nothing on failure.
    -- Load a list of log files.
    return $ BufferLog bufDir bufId
  where
    bufDir = logDir </> T.unpack bufId

-- }}}

-- {{{ Sink

-- | A conduit sink for writing to the given buffer's log.
-- This takes the given buffer log object and automatically writes the lines
-- given to it to the correct log file.
bufferLogSink :: BufferLog -> Sink ChatLogLine (ResourceT IO) ()
bufferLogSink buffer = do
    -- Open the log file for today.
    todayLog <- liftIO logFileForToday
    writeTo todayLog
  where
    bufDir = bufferDir buffer
    -- Writes messages to the given log ID. Automatically switches to a
    -- different log when necessary.
    writeTo logId = do
        -- Open a sink for the given log and connect our file conduit to it.
        fileConduit logId =$$+ logFileSink bufDir logId
        -- When that terminates, it should leave the last line upstream. Read
        -- that to find the log ID we should switch to.
        lastLineM <- await
        case lastLineM of
             Just lastLine -> do
                 -- Get the new log ID from the last line and put it back
                 -- upstream again.
                 let newLogId = logFileForLine lastLine
                 leftover lastLine
                 -- Switch to the new log file.
                 writeTo newLogId
             -- If there's nothing, then we're probably closing the conduit.
             Nothing -> return ()

    -- Checks messages to see if they should be logged to the given file. If
    -- not, terminates and returns the ID of the file to switch to.
    fileConduit logId = do
        lineM <- await
        case lineM of
             Just line ->
                 if logFileForLine line == logId
                    -- If we've got the right log open, write the line to it.
                    then yield line >> fileConduit logId
                    -- Otherwise, put the line back in the input queue and
                    -- terminate.
                    else leftover line
             Nothing -> return ()

-- }}}

