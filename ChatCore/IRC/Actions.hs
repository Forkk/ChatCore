module ChatCore.IRC.Actions where

import qualified Data.Text as T

import ChatCore.IRC.Commands
import ChatCore.IRC.Connection
import ChatCore.Types

pongLine :: Maybe T.Text -> IRCLine
pongLine =
    IRCLine Nothing ICmdPong []

nickLine :: Nick -> IRCLine
nickLine nick =
    IRCLine Nothing ICmdNick [nick] Nothing

userLine :: T.Text -> T.Text -> IRCLine
userLine username realname =
    IRCLine Nothing ICmdUser [username, "*", "*"] (Just realname)


privMsgLine :: ChatDest -> T.Text -> IRCLine
privMsgLine dest msg =
    IRCLine Nothing ICmdPrivmsg [dest] (Just msg)

noticeLine :: ChatDest -> T.Text -> IRCLine
noticeLine dest msg =
    IRCLine Nothing ICmdNotice [dest] (Just msg)


joinLine :: ChatChan -> IRCLine
joinLine chan =
    IRCLine Nothing ICmdJoin [chan] Nothing

partLine :: ChatChan -> T.Text -> IRCLine
partLine chan msg =
    IRCLine Nothing ICmdPart [chan] (Just msg)

