-- | FRP utilities for the IRC library.
module ChatCore.IRC.FRP where

import Control.Lens
import FRP.Sodium
import ChatCore.IRC

-- | Filters the given 
filterCmd :: Event IRCLine -> IRCCommand -> Event IRCLine
filterCmd eLine cmd = filterE ((==cmd) . view ilCommand) eLine

-- | Takes the given IRC command event stream and extracts sources.
filterSource :: Event IRCLine -> Event IRCSource
filterSource = filterJust . fmap (view ilSource)
