{-# LANGUAGE TemplateHaskell #-}
-- | Module for the IRC library's IRC command type definitions.
module ChatCore.IRC.Commands where

import Data.Text
import ChatCore.IRC.TH

defineIRCCmds
    [ ("ICmdPrivmsg", "PRIVMSG")
    , ("ICmdNotice",  "NOTICE")
    
    , ("ICmdJoin", "JOIN")
    , ("ICmdPart", "PART")

    , ("ICmdNick", "NICK")
    , ("ICmdUser", "USER")

    , ("ICmdPing", "PING")
    , ("ICmdPong", "PONG")
    ]

