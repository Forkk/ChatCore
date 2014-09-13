module ChatCore.CoreController where

import Control.Concurrent.Actor

import ChatCore.Types
import {-# SOURCE #-} ChatCore.Protocol

data CoreActorMsg = CoreNewConnection (CoreCtlHandle -> IO (UserId, RemoteClient ()))

instance ActorMessage CoreActorMsg

type CoreCtlHandle = ActorHandle CoreActorMsg

