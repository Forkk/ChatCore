module ChatCore.IRC.Connection
    ( IRCConnection (ircRecvLine, ircConnStatus)
    , connectIRC
    , disconnectIRC
    , nullConn

    , module ChatCore.IRC.Commands
    , module ChatCore.IRC.Line
    ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Error.Util
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import FRP.Sodium
import Network
import System.IO

import ChatCore.Types
import ChatCore.IRC.Commands
import ChatCore.IRC.Line

-- | A data structure containing events and behaviors from an IRC connection.
-- This acts as a handle to a specific connection.
data IRCConnection = IRCConnection
    { ircThread :: Async ()
    , ircRecvLine :: Event IRCLine
    -- | Indicates this connection's status. Note that once disconnected, a
    -- connection cannot be restarted.
    , ircConnStatus :: Behavior ConnectionStatus
    -- | An IO action which tells the IRC thread to disconnect.
    , ircDisconnect :: IO ()
    }

-- | An empty, disconnected, placeholder IRC connection object.
nullConn :: IRCConnection
nullConn = IRCConnection undefined never (pure Disconnected) (return ())

-- | Connects to an IRC server at the given host and port.
-- The connection is started on a separate thread.
-- NOTE: Although the connection runs on another thread, this function should
-- still not be executed within a reactive transaction. Use `executeAsyncIO`
-- instead.
connectIRC :: HostName -> PortID -> Event IRCLine -> IO IRCConnection
connectIRC host port eSendLine = do
    putStrLn "Connecting to IRC."
    (bConnStatus, pushConnStatus) <- sync $ newBehavior Connecting
    (eRecvLine, pushRecvLine) <- sync newEvent
    th <- async $ execIRC host port eSendLine
                          (sync . pushRecvLine) (sync . pushConnStatus)
    return IRCConnection
        { ircThread = th
        , ircRecvLine = eRecvLine
        , ircConnStatus = bConnStatus
        , ircDisconnect = cancel th
        }

disconnectIRC :: IRCConnection -> IO ()
disconnectIRC = ircDisconnect

execIRC :: HostName -> PortID -> Event IRCLine
        -> (IRCLine -> IO ()) -> (ConnectionStatus -> IO ())
        -> IO ()
execIRC host port eSendLine pushRecvLine pushConnStatus = do
    -- TODO: Handle disconnect via cancel.
    hand <- liftIO $ connectTo host port
    cleanup <- sync $ listen eSendLine $ sendLine hand
    pushConnStatus Connected
    inputLoop hand
    cleanup
  where
    inputLoop hand = do
        -- Wait for input for 3 seconds. If no input is available, make sure the
        -- handle is still open.
        isOpen <- hIsOpen hand
        if isOpen
           then do
             recvLine hand >>= pushRecvLine
             inputLoop hand
           else pushConnStatus Disconnected

-- | Sends the given IRC message on the given handle.
sendLine :: Handle -> IRCLine -> IO ()
sendLine hand line =
    B.hPut hand (lineToByteString line `B.append` "\r\n")

-- | Reads an IRC line from the given handle.
-- If no data is available, blocks until one is received.
recvLine :: Handle -> IO IRCLine
recvLine hand = do
    lineBS <- removeCr <$> liftIO (B.hGetLine hand)
    let lineM = hush $ parseLine lineBS
    maybe (recvLine hand) return lineM

-- ircParseError :: ParseError -> IO ()
-- ircParseError e = runStderrLoggingT $
--     $(logWarnS) "IRC" ("Failed to parse IRC message: " <> T.pack (show e))


removeCr :: B.ByteString -> B.ByteString
removeCr str =
    if BC.last str == '\r'
       then B.init str
       else str
