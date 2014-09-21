module ChatCore.UserController
    ( UserCtlHandle
    , UserCtlActorMsg
    , ucSendClientCmd
    , ucSendCoreEvt
    ) where

import Control.Concurrent.Actor
import Control.Monad.IO.Class

import {-# SOURCE #-} ChatCore.Events

type UserCtlHandle = ActorHandle UserCtlActorMsg

data UserCtlActorMsg


ucSendClientCmd :: (MonadIO m) => UserCtlHandle -> ClientCommand -> m ()

ucSendCoreEvt :: (MonadIO m) => UserCtlHandle -> CoreEvent -> m ()

