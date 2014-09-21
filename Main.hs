module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite

import ChatCore.Database
import ChatCore.CoreController

-- TODO: Configurable database settings.
main :: IO ()
main = runStderrLoggingT $ withSqlitePool ":memory:" 5 $ \pool -> do
    (flip runSqlPool $ pool) $ do
        runMigration migrateAll
        user <- insert $ ChatUser "Forkk"
        network <- insert $ IrcNetwork "EsperNet" ["ChatCore"] [] user
        return ()
    runCoreCtl pool

