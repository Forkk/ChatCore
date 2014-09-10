module ChatCore.CoreController where

import Control.Concurrent.Actor

import {-# SOURCE #-} ChatCore.Protocol

data CoreActorMsg
    = forall conn. CoreProtocol conn => CoreNewConnection conn

instance ActorMessage CoreActorMsg

type CoreCtlHandle = ActorHandle CoreActorMsg

