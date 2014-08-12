-- | A state actor is an actor transformed by the state monad. This module
-- provides some functions to make it easier to use such actors.
module ChatCore.Util.StateActor where

import Control.Concurrent.Actor
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Typeable

type StateActorM msg s = StateT s (ActorM msg)
type StateActor msg s = StateActorM msg s ()

-- | Spawns an actor for the given state actor monad and returns its address.
spawnStateActor :: ActorMessage msg =>
                   StateActor msg s -> s -> IO (ActorHandle msg)
spawnStateActor actor state = spawnActor $ runStateActor actor state

-- | Runs the given state actor with the given state.
runStateActor :: ActorMessage msg =>
                 StateActor msg s -> s -> ActorM msg ()
runStateActor actor state = runStateT actor state >> return ()

-- | Executes the given state actor with the given state, returning the final
-- state.
execStateActor :: ActorMessage msg =>
                  StateActor msg s -> s -> ActorM msg s
execStateActor = execStateT

-- | Executes the given state actor with the given state, returning the actor's
-- return value and discarding the final state.
evalStateActor :: ActorMessage msg =>
                  StateActor msg s -> s -> ActorM msg ()
evalStateActor = evalStateT

-- | Creates a Handler from the given function and the given state.
-- The created handler will call the function to handle the message and then
-- run the given actor with the modified state.
-- stateActorHandler :: forall m. forall s. Typeable m => StateActor s -> s -> (m -> StateActor s) -> Handler
-- stateActorHandler actor state handler =
--     Case wrapped
--   where
--     wrapped msg = evalStateActor (handler msg >> actor) state

