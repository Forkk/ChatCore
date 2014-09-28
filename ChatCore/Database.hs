module ChatCore.Database where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Word
import qualified Data.Text as T
import Database.Persist.Sql
import Database.Persist.TH

import ChatCore.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ChatUser
    name        UserName
    UniqueUserName name
    deriving Show

IrcNetwork
    name        ChatNetworkName
    nicks       [Nick]
    channels    [ChatChan]
    user        ChatUserId
    UniqueNetName user name
    deriving Show

IrcServer
    address     T.Text
    port        Word16
    networkId   IrcNetworkId
    deriving Show
|]

-- | Gets  a list of usernames that exist.
getUserNames :: (Monad m, MonadIO m, MonadBaseControl IO m) =>
                SqlPersistT m [UserName]
getUserNames = map (chatUserName . entityVal) <$> selectList [] []

-- | Class for contexts where the database can be accessed.
class (Monad m, MonadBaseControl IO m) => MonadDB m where
    getConnPool :: m ConnectionPool
    runDB :: SqlPersistT m a -> m a
    runDB t = do
        pool <- getConnPool
        runSqlPool t pool
