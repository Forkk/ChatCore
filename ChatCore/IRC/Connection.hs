module ChatCore.IRC.Connection
    ( IRCConnection
    , connectIRC
    , evalIRCAction
    , doIRC

    , IRC

    , sendLine
    , recvLine

    , module ChatCore.IRC.Commands
    , module ChatCore.IRC.Line
    ) where

import Control.Applicative
import Control.Monad.STM
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM.TMChan
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Error.Util
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import Data.Conduit.TMChan
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Network
import System.IO

import ChatCore.Types
import ChatCore.IRC.Commands
import ChatCore.IRC.Line


-- A handle referring to an IRC connection. Contains the channels for reading
-- and writing as well as a reference to the connection's thread.
data IRCConnection = IRCConnection
    { ircSendChan   :: TMChan IRCLine   -- Channel for messages to send to the IRC server.
    , ircRecvChan   :: TMChan IRCLine   -- Channel for messages received from the IRC server.
    , ircSendThread     :: ThreadId     -- The connection thread's ID.
    , ircRecvThread     :: ThreadId     -- The connection thread's ID.
    }


data IRCState = IRCState
    { ircServer     :: (HostName, PortID)   -- The hostname and port of the server.
    , ircHandle     :: Handle               -- The handle for the IRC connection.
    , stateSendChan :: TMChan IRCLine
    , stateRecvChan :: TMChan IRCLine
    }

-- Monad for the internal IRC stuff.
type IRC = ReaderT IRCConnection IO


-- Executes an IRC action with the given IRC connection.
evalIRCAction :: IRC a -> IRCConnection -> IO a
evalIRCAction = runReaderT

-- A version of evalIRCAction with its arguments flipped.
doIRC :: IRCConnection -> IRC a -> IO a
doIRC = flip evalIRCAction


-- | Connects to an IRC server at the given host and port.
connectIRC :: HostName -> PortID -> IO IRCConnection
connectIRC host port = do
    -- Create channels.
    sendChan <- atomically $ newTMChan
    recvChan <- atomically $ newTMChan

    -- Connect
    handle <- connectTo host port

    -- Start the sender thread.
    sendThread <- forkIO $ do
        -- Read from the channel and call sendIRCMessage for each item.
        sourceTMChan sendChan $$ sendMessages handle
    recvThread <- forkIO $ do
        -- Read from the handle, parse each message, and write them to the channel.
        receiveMessages handle $$ sinkTMChan recvChan True

    return IRCConnection
        { ircSendChan = sendChan
        , ircRecvChan = recvChan
        , ircSendThread = sendThread
        , ircRecvThread = recvThread
        }


removeCr :: B.ByteString -> B.ByteString
removeCr str =
    if BC.last str == '\r'
       then B.init str
       else str

-- | Provides a Source of received IRC messages on the given handle.
receiveMessages :: Handle -> Source IO IRCLine
receiveMessages handle =
    -- Read lines from the handle and parse them.
    CB.sourceHandle handle $= CB.lines $= CL.map removeCr $= CL.map parseLine $= CL.mapMaybe hush

-- | Sends the given IRC message on the given handle.
sendMessages :: Handle -> Sink IRCLine IO ()
sendMessages handle =
    -- Convert the lines to bytestrings and write them to the handle.
    CL.map lineToByteString =$ CL.map (`B.append` "\r\n") =$ CB.sinkHandle handle


-- | Gets the next line received from the IRC server.
-- If no line has been received, blocks until one is received.
recvLine :: IRC IRCLine
recvLine = do
    -- FIXME: This crashes when the channel is closed.
    recvChan <- asks ircRecvChan
    mLine <- lift $ atomically $ readTMChan recvChan
    return $ fromJust mLine

-- | Sends a line to the IRC server.
sendLine :: IRCLine -> IRC ()
sendLine line = do
    sendChan <- asks ircSendChan
    lift $ atomically $ writeTMChan sendChan line


-- | Function used primarily for testing which prints all lines received on the
-- given connection.
printReceivedLines :: IRCConnection -> IO ()
printReceivedLines conn = (sourceTMChan $ ircRecvChan conn) $$ CL.mapM_ print

