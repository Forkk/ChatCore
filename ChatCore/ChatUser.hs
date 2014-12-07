-- | The user module, for user logic.
module ChatCore.ChatUser
    ( ChatUser (..)
    , chatUserName
    , bUserNetworks
    , bUserClients

    , chatUser
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Acid
import qualified Data.IxSet as I
import qualified Data.Map as M
import Data.Monoid
import FRP.Sodium
import FRP.Sodium.IO

import ChatCore.Events
import ChatCore.ChatNetwork
import ChatCore.ClientProtocol
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
-- Networks
--------------------------------------------------------------------------------

behNetworks :: AcidState ChatCoreState
            -> ChatUserName
            -> Event ChatCoreNetwork
            -> Event ClientCommand
            -> Reactive (Behavior (I.IxSet ChatNetwork))
behNetworks acid uName eNewNetwork eClientCmd =
    accum I.empty eDoAddNetwork
  where
    initNet netSt = chatNetwork uName netSt acid
                    $ filterE (isForNetwork (netSt ^. netStName)) eClientCmd
    eDoAddNetwork = I.insert <$> execute (initNet <$> eNewNetwork)

-- | True if the given client command should be handled by the given network.
isForNetwork :: ChatNetworkName -> ClientCommand -> Bool
isForNetwork netName (SendMessage { sendMsgNetwork = netName' }) = netName == netName'
isForNetwork netName (JoinChannel netName' _) = netName == netName'
isForNetwork netName (PartChannel netName' _ _) = netName == netName'
-- isForNetwork _ _ = False


--------------------------------------------------------------------------------
-- Clients
--------------------------------------------------------------------------------

behRemoteClients :: ChatUserName
                 -> Event RemoteClient
                 -> Behavior (I.IxSet ChatNetwork)
                 -> Event CoreEvent
                 -> Reactive (Behavior (M.Map Int RemoteClientInfo), IO ())
behRemoteClients uName eNewConn bNetworks eCoreEvent = do
    rec
      -- Fires when a client disconnects.
      let eClientDisconnect :: Event (Int, RemoteClientInfo)
          eClientDisconnect = switchMergeWith mkDCEvent (M.toList <$> bClientMap)

      -- Fires the ID of a client when it disconnects.
      let eClientDisconnectId :: Event Int
          eClientDisconnectId = fst <$> eClientDisconnect

      -- Event streams of functions to add and remove clients.
      let eDoAddClient = uncurry M.insert <$> eNewClient
          eDoRemoveClient = M.delete <$> eClientDisconnectId

      -- Starts new clients and fires events for each client started.
      eNewClient <- tagIds $ executeAsyncIO (($ clientCtx) <$> eNewConn)

      -- The client list is a map that is modified by functions coming out of the
      -- `eDo(Add|Remove)Client` events.
      bClientMap <- accum M.empty (eDoAddClient <> eDoRemoveClient)

    -- Cleanup clients when they disconnect.
    clean <- listen eClientDisconnect cleanClient

    return (bClientMap, clean)
  where
    clientCtx = RemoteClientCtx uName eCoreEvent bNetworks
    -- | Fires an event when the given client disconnects.
    mkDCEvent :: (Int, RemoteClientInfo) -> Event (Int, RemoteClientInfo)
    mkDCEvent (cid, client) = const (cid, client) <$> rcDisconnect client
    -- | Runs cleanup actions for the given client.
    cleanClient :: (Int, RemoteClientInfo) -> IO ()
    cleanClient (cid, client) = do
        putStrLn ("Client " <> show cid <> " disconnected.")
        -- FIXME: If the client's cleanup action calls sync, this will lock up.
        cleanupRemoteClient client


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- | Set up behaviors and events for the given user.
chatUser :: ChatCoreUser
         -> AcidState ChatCoreState
         -> Event RemoteClient -- ^ New clients for this user.
         -> Reactive ChatUser
chatUser user acid eNewConn = do
    (eNewNetwork, pushNewNetwork) <- newEvent
    let uName = user ^. usrStName
    rec
      -- Networks.
      bNetworks <- behNetworks acid uName eNewNetwork eClientCmd
      let bNetworkList = I.toList <$> bNetworks

      -- Receive core events from networks.
      let eCoreEvent = switchMergeWith (view eNetworkCoreEvt) bNetworkList


      -- Remote clients.
      (bClients, cleanClients) <- behRemoteClients uName eNewConn bNetworks eCoreEvent
      let bClientList = map snd <$> M.toList <$> bClients

      -- Receive client commands from all clients.
      let eClientCmd = switchMergeWith rcCommands bClientList
    
    -- FIXME: Removing this line causes core events to not be received by clients.
    _ <- listen eCoreEvent print

    let bCleanupNetworks = map cleanupChatNetwork <$> bNetworkList
        cleanup = do
            cleanClients
            -- Run cleanup actions for all the networks.
            join (sequence_ <$> sync (sample bCleanupNetworks))

    -- Push an add network event for all the networks.
    mapM_ pushNewNetwork $ I.toList (user ^. usrStNetworks)
    return $ ChatUser uName bNetworks bClientList cleanup

