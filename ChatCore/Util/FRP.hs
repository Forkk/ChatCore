module ChatCore.Util.FRP where

import Control.Applicative
import FRP.Sodium


-- | Tags each of the given events with a unique, incrementing integer ID.
tagIds :: Event a -> Reactive (Event (Int, a))
tagIds eInput = do
    let tag evt eid = (eid, evt)
    bCount <- accum 0 (const (+1) <$> eInput)
    return $ snapshot tag eInput bCount
