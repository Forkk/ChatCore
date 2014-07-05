module ChatCore.IRC where

import Control.Monad.STM
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM.TMChan
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Conduit
import Data.Conduit.TMChan
import qualified Data.Conduit.List as CL
import Network
import System.IO

import ChatCore.Types
import ChatCore.IRC.Line


-- A handle referring to an IRC connection. Contains the channels for reading
-- and writing as well as a reference to the connection's thread.
data IRCConnection = IRCConnection
    { ircSendChan   :: TMChan IRCMessage  -- Channel for messages to send to the IRC server.
    , ircRecvChan   :: TMChan IRCMessage  -- Channel for messages received from the IRC server.
    , ircSendThread     :: ThreadId       -- The connection thread's ID.
    , ircRecvThread     :: ThreadId       -- The connection thread's ID.
    }


data IRCState = IRCState
    { ircServer     :: (HostName, PortID)   -- The hostname and port of the server.
    , ircHandle     :: Handle               -- The handle for the IRC connection.
    , stateSendChan :: TMChan IRCMessage
    , stateRecvChan :: TMChan IRCMessage
    }

-- Monad for the internal IRC stuff.
type IRC = StateT IRCConnection IO


-- Executes an IRC action with the given IRC connection.
evalIRCAction :: IRC a -> IRCConnection -> IO a
evalIRCAction = evalStateT


-- | Connects to an IRC server at the given host and port.
connectIRC :: HostName -> PortID -> Nick -> IO IRCConnection
connectIRC host port nick = do
    -- Create channels.
    sendChan <- atomically $ newTMChan
    recvChan <- atomically $ newTMChan

    -- Connect
    handle <- connectTo host port

    -- Start the sender thread.
    sendThread <- forkIO $ do
        -- Read from the channel and call sendIRCMessage for each item.
        sourceTMChan sendChan $$ CL.mapM_ (sendIRCMessage handle)
    recvThread <- forkIO $ do
        -- Read from the handle, parse each message, and write them to the channel.
        receiveMessages handle $$ sinkTMChan recvChan True
    
    return IRCConnection
        { ircSendChan = sendChan
        , ircRecvChan = recvChan
        , ircSendThread = sendThread
        , ircRecvThread = recvThread
        }

-- | Provides a Source of received IRC messages on the given handle.
receiveMessages :: Handle -> Source IO IRCMessage
receiveMessages _ = yield $ PrivMsg "#test" "Test" "This is a test."

-- | Sends the given IRC message on the given handle.
sendIRCMessage :: Handle -> IRCMessage -> IO ()
sendIRCMessage _ msg = putStrLn $ show $ msg


-- | Send a PRIVMSG to the given destination.
sendPrivMsg :: ChatDest -> T.Text -> IRC ()
sendPrivMsg (DestChan chan) msg = lift $ T.putStrLn ("PRIVMSG to channel '" `T.append` chan `T.append` "': " `T.append` msg)

-- | IRC message type. These represent messages sent from the IRC server.
data IRCMessage =
    -- | Represents a received PRIVMSG.
    PrivMsg
        { privmsgSource     :: ChatSource   -- The source this message was received on (channel or user PM).
        , privmsgSender     :: Nick         -- The nick of the user who sent the privmsg.
        , privmsgContent    :: T.Text       -- The content of the privmsg.
        }
    deriving (Show)


