-- | Module with functions for sending specific commands to the IRC server.
module ChatCore.IRC.Commands where

import qualified Data.Text as T

import ChatCore.IRC
import ChatCore.IRC.Line
import ChatCore.Types

sendPongCmd :: Maybe T.Text -> IRC ()
sendPongCmd host =
    sendLine $ IRCLine Nothing (IRCCommand "PONG") [] host

sendNickCmd :: Nick -> IRC ()
sendNickCmd nick =
    sendLine $ IRCLine Nothing (IRCCommand "NICK") [nick] Nothing

sendUserCmd :: T.Text -> T.Text -> IRC ()
sendUserCmd username realname =
    sendLine $ IRCLine Nothing (IRCCommand "USER") [username, "*", "*"] (Just realname)


sendPrivMsgCmd :: ChatDest -> T.Text -> IRC ()
sendPrivMsgCmd dest msg =
    sendLine $ IRCLine Nothing (IRCCommand "PRIVMSG") [dest] (Just msg)

sendNoticeCmd :: ChatDest -> T.Text -> IRC ()
sendNoticeCmd dest msg =
    sendLine $ IRCLine Nothing (IRCCommand "NOTICE") [dest] (Just msg)


sendJoinCmd :: ChatChan -> IRC ()
sendJoinCmd chan =
    sendLine $ IRCLine Nothing (IRCCommand "JOIN") [chan] Nothing

sendPartCmd :: ChatChan -> T.Text -> IRC ()
sendPartCmd chan msg =
    sendLine $ IRCLine Nothing (IRCCommand "PART") [chan] (Just msg)

