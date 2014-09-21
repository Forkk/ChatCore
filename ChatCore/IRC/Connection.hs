module ChatCore.IRC.Connection
    ( IRCConnection
    , connectIRC
    , disconnectIRC

    , MonadIRC (..)

    , sendLine
    , recvLine
    , sourceRecvLine

    , module ChatCore.IRC.Commands
    , module ChatCore.IRC.Line
    ) where

import Control.Applicative
import Control.Exception
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM.TMChan
import Control.Monad.Base
import Control.Monad.Reader
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


type MonadIRCSuper m = (Functor m, Applicative m, Monad m, MonadIO m)

-- | Class for monads that can run IRC actions.
class (MonadIRCSuper m) => MonadIRC m where
    -- | Gets the current IRC connection.
    ircConn :: m (IRCConnection)

instance (MonadIRCSuper m) => MonadIRC (ReaderT IRCConnection m) where
    ircConn = ask


-- A handle referring to an IRC connection. Contains the channels for reading
-- and writing as well as a reference to the connection's thread.
data IRCConnection = IRCConnection
    { ircServer     :: (HostName, PortID)   -- The hostname and port of the server.
    , ircHandle     :: Handle               -- The handle for the IRC connection.
    }


data IRCState = IRCState


-- | Connects to an IRC server at the given host and port.
connectIRC :: (MonadIO m) => HostName -> PortID -> m IRCConnection
connectIRC host port = do
    -- Connect
    handle <- liftIO $ connectTo host port
    return IRCConnection
        { ircServer = (host, port)
        , ircHandle = handle
        }

disconnectIRC :: (MonadIO m) => IRCConnection -> m ()
disconnectIRC conn = liftIO $ hClose $ ircHandle $ conn


removeCr :: B.ByteString -> B.ByteString
removeCr str =
    if BC.last str == '\r'
       then B.init str
       else str

-- | Sends the given IRC message on the given handle.
sendLine :: (MonadIRC m) => IRCLine -> m ()
sendLine line = do
    handle <- ircHandle <$> ircConn
    liftIO $ B.hPut handle (lineToByteString line `B.append` "\r\n")

-- | Gets the next line received from the IRC server.
-- If no line has been received, blocks until one is received.
recvLine :: (MonadIRC m) => m (Maybe IRCLine)
recvLine = do
    handle <- ircHandle <$> ircConn
    lineBS <- removeCr <$> (liftIO $ B.hGetLine handle)
    return $ hush $ parseLine lineBS

-- | Creates a conduit source of IRC lines received.
-- This can be run on another thread to receive messages in the background.
sourceRecvLine :: (MonadIRC m) => m (Source IO IRCLine)
sourceRecvLine = do
    handle <- ircHandle <$> ircConn
    -- Read lines from the handle and parse them.
    return $ CB.sourceHandle handle
          $= CB.lines $= CL.map removeCr
          $= CL.map parseLine $= CL.mapMaybe hush

