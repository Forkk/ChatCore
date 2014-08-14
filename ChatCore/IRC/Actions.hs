-- | Module with functions for sending specific commands to the IRC server.
module ChatCore.IRC.Actions where

import qualified Data.Text as T

import ChatCore.IRC.Commands
import ChatCore.IRC.Connection
import ChatCore.IRC.Line
import ChatCore.Types

sendPongCmd :: Maybe T.Text -> IRC ()
sendPongCmd host =
    sendLine $ IRCLine Nothing ICmdPong [] host

sendNickCmd :: Nick -> IRC ()
sendNickCmd nick =
    sendLine $ IRCLine Nothing ICmdNick [nick] Nothing

sendUserCmd :: T.Text -> T.Text -> IRC ()
sendUserCmd username realname =
    sendLine $ IRCLine Nothing ICmdUser [username, "*", "*"] (Just realname)


sendPrivMsgCmd :: ChatDest -> T.Text -> IRC ()
sendPrivMsgCmd dest msg =
    sendLine $ IRCLine Nothing ICmdPrivmsg [dest] (Just msg)

sendNoticeCmd :: ChatDest -> T.Text -> IRC ()
sendNoticeCmd dest msg =
    sendLine $ IRCLine Nothing ICmdNotice [dest] (Just msg)


sendJoinCmd :: ChatChan -> IRC ()
sendJoinCmd chan =
    sendLine $ IRCLine Nothing ICmdJoin [chan] Nothing

sendPartCmd :: ChatChan -> T.Text -> IRC ()
sendPartCmd chan msg =
    sendLine $ IRCLine Nothing ICmdPart [chan] (Just msg)

