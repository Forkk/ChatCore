{-# LANGUAGE OverloadedStrings #-}
module ChatCore.IRC where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import ChatCore.Types

-- IRC monad for doing IRC things.
-- TODO: Implement this.
type IRC = IO

-- | Send a PRIVMSG to the given destination.
sendPrivMsg :: ChatDest -> T.Text -> IRC ()
sendPrivMsg (DestChan chan) msg = T.putStrLn ("PRIVMSG to channel '" `T.append` chan `T.append` "': " `T.append` msg)


-- | IRC message type. These represent messages sent from the IRC server.
data IRCMessage =
    -- | Represents a received PRIVMSG.
    ReceivedPrivMsg
        { privmsgSource     :: ChatSource   -- The source this message was received on (channel or user PM).
        , privmsgSender     :: Nick         -- The nick of the user who sent the privmsg.
        , privmsgContent    :: T.Text       -- The content of the privmsg.
        }

