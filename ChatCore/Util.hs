-- | Module for general utility functions.
module ChatCore.Util
    ( concatMapM
    , dropMaybeT
    ) where

import Control.Monad
import Control.Monad.Trans.Maybe

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

dropMaybeT :: (Monad m) => MaybeT m a -> m ()
dropMaybeT mt = runMaybeT mt >> return ()

