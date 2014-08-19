-- | Module for general utility functions.
module ChatCore.Util where

import Control.Monad

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

