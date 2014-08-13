-- | Module with functions for sending specific commands to the IRC server.
module ChatCore.IRC.Actions where

import qualified Data.Text as T

import ChatCore.IRC.Commands
import ChatCore.IRC.Connection
import ChatCore.IRC.Line
import ChatCore.Types

sendPongCmd :: Maybe T.Text -> IRC ()
sendPongCmd host =
    sendLine $ IRCLine Nothing ICPong [] host

sendNickCmd :: Nick -> IRC ()
sendNickCmd nick =
    sendLine $ IRCLine Nothing ICNick [nick] Nothing

sendUserCmd :: T.Text -> T.Text -> IRC ()
sendUserCmd username realname =
    sendLine $ IRCLine Nothing ICUser [username, "*", "*"] (Just realname)


sendPrivMsgCmd :: ChatDest -> T.Text -> IRC ()
sendPrivMsgCmd dest msg =
    sendLine $ IRCLine Nothing ICPrivmsg [dest] (Just msg)

sendNoticeCmd :: ChatDest -> T.Text -> IRC ()
sendNoticeCmd dest msg =
    sendLine $ IRCLine Nothing ICNotice [dest] (Just msg)


sendJoinCmd :: ChatChan -> IRC ()
sendJoinCmd chan =
    sendLine $ IRCLine Nothing ICJoin [chan] Nothing

sendPartCmd :: ChatChan -> T.Text -> IRC ()
sendPartCmd chan msg =
    sendLine $ IRCLine Nothing ICPart [chan] (Just msg)

