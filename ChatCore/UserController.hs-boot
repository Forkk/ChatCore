module ChatCore.UserController
    ( UserCtlHandle
    , UserCtlActorMsg (..)
    , ucSendClientCmd
    , ucSendCoreEvt
    ) where

import Control.Concurrent.Actor

import {-# SOURCE #-} ChatCore.Events

type UserCtlHandle = ActorHandle UserCtlActorMsg

data UserCtlActorMsg


ucSendClientCmd :: ActorMessage m => UserCtlHandle -> ClientCommand -> ActorM m ()

ucSendCoreEvt :: ActorMessage m => UserCtlHandle -> CoreEvent -> ActorM m ()

