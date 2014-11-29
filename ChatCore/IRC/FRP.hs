-- | FRP utilities for the IRC library.
module ChatCore.IRC.FRP where

import Control.Applicative
import Control.Lens
import qualified Data.Text as T
import FRP.Sodium

import ChatCore.IRC
import ChatCore.Types

-- | Filters the given 
filterCmd :: Event IRCLine -> IRCCommand -> Event IRCLine
filterCmd eLine cmd = filterE ((==cmd) . view ilCommand) eLine

-- | Takes the given IRC command event stream and extracts sources.
filterSource :: Event IRCLine -> Event IRCSource
filterSource = filterJust . fmap (view ilSource)

-- Filters only IRC lines where the source is the current user.
filterSelfSource :: Behavior Nick -> Event IRCLine -> Event IRCLine
filterSelfSource bNick eLine =
    filterJust (snapshot doFilter eLine bNick)
  where
    doFilter line nick =
        if srcNick line == Just nick
           then Just line
           else Nothing
    srcNick = preview (ilSource . _Just . _UserSource . iuNick)

-- Filters out the first argument of the given lines.
filterFirstArg :: Event IRCLine -> Event T.Text
filterFirstArg eLine = filterJust (preview (ilArgs . _head) <$> eLine)
