{-# LANGUAGE TemplateHaskell #-}
-- | Module for the IRC library's IRC command type definitions.
module ChatCore.IRC.Commands where

import Data.Text
import ChatCore.Util.KeyedEnum

mkKeyedEnum "IRCCommand" "icmdFromStr" "icmdToStr" (Just "ICmdOther")
    [ ("ICmdPrivmsg", "PRIVMSG" :: String)
    , ("ICmdNotice",  "NOTICE")
    
    , ("ICmdJoin", "JOIN")
    , ("ICmdPart", "PART")
    , ("ICmdQuit", "QUIT")

    , ("ICmdNick", "NICK")
    , ("ICmdUser", "USER")

    , ("ICmdPing", "PING")
    , ("ICmdPong", "PONG")
    ]

