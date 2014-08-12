module ChatCore.Util.ActorUtil where

import Control.Concurrent.Actor
import Data.Typeable

-- -- | Instance for actor handles.
-- -- Actor handles are opaque data structures which hold the actor's address and
-- -- some other information about the actor. They allow for easily building
-- -- abstractions on top of the actor.
-- class ActorHandle hand where
--     -- | Gets the actor address for the given handle.
--     actorAddr :: hand -> Address

-- -- An address is also a handle.
-- instance ActorHandle Address where
--     actorAddr = id


-- linkActorHandle :: ActorHandle hand => hand -> Actor
-- linkActorHandle = link . actorAddr

-- -- | Sends the given message to the given actor handle.
-- sendActorMsg :: (ActorHandle hand, Typeable msg) => hand -> msg -> Actor
-- sendActorMsg hand msg = send (actorAddr hand) msg

-- -- Flipped version of `sendActorMsg`
-- msgActorSend :: (ActorHandle hand, Typeable msg) => msg -> hand -> Actor
-- msgActorSend = flip sendActorMsg

-- -- | Sends the given message from within the IO monad.
-- -- This is done asynchronously.
-- -- This is a hack and should really only be used for testing.
-- sendActorMsgIO :: (ActorHandle hand, Typeable msg) => hand -> msg -> IO ()
-- sendActorMsgIO h m = (spawn $ sendActorMsg h m) >> return ()

