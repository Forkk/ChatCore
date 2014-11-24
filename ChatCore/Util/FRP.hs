module ChatCore.Util.FRP where

import Control.Applicative
import FRP.Sodium


-- | Tags each of the given events with a unique, incrementing integer ID.
tagIds :: Event a -> Reactive (Event (Int, a))
tagIds eInput = do
    let tag evt eid = (eid, evt)
    bCount <- accum 0 (const (+1) <$> eInput)
    return $ snapshot tag eInput bCount


-- | Switches with the given function.
switchWith :: (a -> Behavior b) -> Behavior a -> Reactive (Behavior b)
switchWith func beh = switch (func <$> beh)

-- | Switches events with the given function.
switchWithE :: (a -> Event b) -> Behavior a -> Event b
switchWithE func beh = switchE (func <$> beh)


-- | Takes a behavior of a list of events and produces a single event stream.
switchMerge :: Behavior [Event a] -> Event a
switchMerge = switchMergeWith id

-- | Like `switchMerge`, but applies a function to the list elements first.
switchMergeWith :: (a -> Event b) -> Behavior [a] -> Event b
switchMergeWith func behs = switchE (foldr merge never <$> map func <$> behs)
