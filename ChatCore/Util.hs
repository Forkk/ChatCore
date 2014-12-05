-- | Module for general utility functions.
module ChatCore.Util where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.Text as T

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

dropMaybeT :: (Applicative m, Monad m) => MaybeT m a -> m ()
dropMaybeT mt = void (runMaybeT mt)

-- | Flipped version of execStateT
stateExec :: Monad m => s -> StateT s m a -> m s
stateExec = flip execStateT

-- | Flipped version of evalStateT
stateEval :: Monad m => s -> StateT s m a -> m a
stateEval = flip evalStateT

-- | `show` for text.
-- Basically, @T.pack . show@.
tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

