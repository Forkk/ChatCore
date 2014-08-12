module ChatCore.UserController where

import Control.Concurrent.Actor

import ChatCore.Events
import ChatCore.Protocol


data UserCtlActorMsg
    = UserCtlCoreEvent CoreEvent
    | UserCtlClientCommand ClientCommand
    | UserCtlNewConnection ClientConnection

instance ActorMessage UserCtlActorMsg

type UserCtlHandle = ActorHandle UserCtlActorMsg

