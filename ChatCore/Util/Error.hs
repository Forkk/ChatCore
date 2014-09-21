module ChatCore.Util.Error where

import Control.Exception
import Control.Monad

-- | Runs the given IO action and wraps the return value in Nothing if an
-- exception is raised.
tryMaybe :: IO a -> IO (Maybe a)
tryMaybe f = catch (Just `liftM` f) (return . catchNothing)
  where
    catchNothing :: SomeException -> Maybe a
    catchNothing _ = Nothing

