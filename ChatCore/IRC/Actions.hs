-- | Module with functions for sending specific commands to the IRC server.
module ChatCore.IRC.Actions where

import qualified Data.Text as T

import ChatCore.IRC.Commands
import ChatCore.IRC.Connection
import ChatCore.IRC.Line
import ChatCore.Types

sendPongCmd :: (MonadIRC m) => Maybe T.Text -> m ()
sendPongCmd host =
    sendLine $ IRCLine Nothing ICmdPong [] host

sendNickCmd :: (MonadIRC m) => Nick -> m ()
sendNickCmd nick =
    sendLine $ IRCLine Nothing ICmdNick [nick] Nothing

sendUserCmd :: (MonadIRC m) => T.Text -> T.Text -> m ()
sendUserCmd username realname =
    sendLine $ IRCLine Nothing ICmdUser [username, "*", "*"] (Just realname)


sendPrivMsgCmd :: (MonadIRC m) => ChatDest -> T.Text -> m ()
sendPrivMsgCmd dest msg =
    sendLine $ IRCLine Nothing ICmdPrivmsg [dest] (Just msg)

sendNoticeCmd :: (MonadIRC m) => ChatDest -> T.Text -> m ()
sendNoticeCmd dest msg =
    sendLine $ IRCLine Nothing ICmdNotice [dest] (Just msg)


sendJoinCmd :: (MonadIRC m) => ChatChan -> m ()
sendJoinCmd chan =
    sendLine $ IRCLine Nothing ICmdJoin [chan] Nothing

sendPartCmd :: (MonadIRC m) => ChatChan -> T.Text -> m ()
sendPartCmd chan msg =
    sendLine $ IRCLine Nothing ICmdPart [chan] (Just msg)

