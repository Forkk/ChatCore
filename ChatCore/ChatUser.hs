-- | The user module, for user logic.
module ChatCore.ChatUser where

import Control.Applicative
import Control.Lens
import Control.Monad
import Crypto.PasswordStore
import Data.Acid
import qualified Data.ByteString as B
import qualified Data.IxSet as I
import qualified Data.Map as M
import Data.Monoid
import Data.Traversable hiding (mapM, sequence)
import FRP.Sodium
import FRP.Sodium.IO

import ChatCore.Events
import ChatCore.ChatNetwork
import ChatCore.Protocol
import ChatCore.State
import ChatCore.Types
import ChatCore.Util.FRP

-- | The main state object for users.
data ChatUser = ChatUser
    { _chatUserName :: ChatUserName
    , _bUserNetworks :: Behavior (I.IxSet ChatNetwork)
    , _bUserClients :: Behavior [RemoteClientInfo]
    , cleanupChatUser :: IO ()
    }
$(makeLenses ''ChatUser)


--------------------------------------------------------------------------------
-- Reactive
--------------------------------------------------------------------------------

-- | Set up behaviors and events for the given user.
chatUser :: ChatCoreUser
         -> AcidState ChatCoreState
         -> Event RemoteClient -- ^ New clients for this user.
         -> IO ChatUser
chatUser user acid eNewConn = do
    (eNewNetwork, pushNewNetwork) <- sync newEvent
    let uname = user ^. usrStName
    chatUsr <- sync $ do
      rec
        let eCoreEvent = never

        let initNet netSt = chatNetwork uname netSt acid
                            $ filterE (isForNetwork (netSt ^. netStName)) eClientCmd
            eAddNetwork = I.insert <$> executeAsyncIO (initNet <$> eNewNetwork)

        bNetworks <- accum I.empty eAddNetwork
        let bNetworkList = I.toList <$> bNetworks
        bNetworkInfos <- switch (sequenceA <$> map getNetworkInfo <$> bNetworkList)

        -- Password
        -- let eSetPassword = never
        -- bPassHash <- hold (user ^. usrStPassword) $
        --              executeSyncIO (hashPassword <$> eSetPassword)


        --------------------------------------------------------------------------------
        -- Remote Clients
        --------------------------------------------------------------------------------

        let clientCtx = RemoteClientCtx uname eCoreEvent bNetworkInfos

        -- Event to add new clients.
        eNewClient <- tagIds $ executeAsyncIO (($ clientCtx) <$> eNewConn)
        let eAddClient = uncurry M.insert <$> eNewClient

        -- Add new clients to the list.
        bClientMap <- accum M.empty eAddClient
        let bClients = map snd <$> M.toList <$> bClientMap

        -- Receive client commands from all clients.
        let eClientCmd = switchE (foldr merge never <$> map rcCommands <$> bClients)
        
        cleanNCPrint <- listen (fst <$> eNewClient) $ \cid ->
                              putStrLn ("New client with ID: " <> show cid)

      let bCleanupNetworks = map cleanupChatNetwork <$> bNetworkList
          cleanup = do
              cleanNCPrint
              -- Run cleanup actions for all the networks.
              join (sequence_ <$> sync (sample bCleanupNetworks))
      return $ ChatUser uname bNetworks bClients cleanup

    -- Push an add network event for all the networks.
    mapM_ (sync . pushNewNetwork) $ I.toList (user ^. usrStNetworks)
    return chatUsr


-- | Starts a new remote client.
userClient :: RemoteClientCtx -> RemoteClient -> IO RemoteClientInfo
userClient ctx client = client ctx


-- | True if the given client command should be handled by the given network.
isForNetwork :: ChatNetworkName -> ClientCommand -> Bool
isForNetwork netName (SendMessage { sendMsgNetwork = netName' }) = netName == netName'
isForNetwork netName (JoinChannel netName' _) = netName == netName'
isForNetwork netName (PartChannel netName' _ _) = netName == netName'
-- isForNetwork _ _ = False


-- | Hashes and salts the given password.
hashPassword :: B.ByteString -> IO B.ByteString
hashPassword pass = makePassword pass 24
