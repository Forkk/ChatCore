module ChatCore.NetworkController where

import Control.Concurrent.Actor

import ChatCore.Events
import ChatCore.IRC.Line

data NetCtlActorMsg
    -- = NetCtlClientCmd   ClientCommand
    = NetCtlIRCLine     IRCLine

instance ActorMessage NetCtlActorMsg

type NetCtlHandle = ActorHandle NetCtlActorMsg

