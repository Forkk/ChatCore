-- | The core module defines data structures and actions related to the core state.
module ChatCore.Core where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Data.Acid
import qualified Data.Map as M
import qualified Data.Text as T
import FRP.Sodium
import FRP.Sodium.IO
import Network

import ChatCore.ChatUser
import ChatCore.ClientProtocol
import ChatCore.ClientProtocol.JSON
import ChatCore.State
import ChatCore.Types
import ChatCore.Util.FRP

connListeners :: [IO ConnListener]
connListeners =
    [ jsonConnListener (PortNumber 1337)
    ]


--------------------------------------------------------------------------------
-- Clients
--------------------------------------------------------------------------------


-- | Creates an event for when a client attaches to the given user.
evtUserClientAttach :: ChatUserName
                    -> Behavior (M.Map Int ActivePendingClient)
                    -> Event RemoteClient
evtUserClientAttach uName bPendingClientMap =
    snd <$> eAttach
  where
    eAttach = filterE ((==uName) . fst) $ switchMerge bAttachEvts
    bAttachEvts = map (apcAttach . snd) <$> M.toList <$> bPendingClientMap


-- | Creates a behavior which holds a list of pending clients.
behPendingClients :: AcidState ChatCoreState
                  -> [ConnListener]
                  -> Reactive (Behavior (M.Map Int ActivePendingClient))
behPendingClients acid listeners = do
  rec
    eNewPending <- evtNewPending acid $ foldr (merge . clNewConn) never listeners

    -- A stream of events which insert or remove pending clients in the pending
    -- client list.
    let eDoAddPending = uncurry M.insert <$> eNewPending
        -- Remove any client that requests attachment. To do this, we just take
        -- the first element of each attach request tuple.
        eDoRemovePending = M.delete <$> evtRemovePending bPendingClients

    -- A map of IDs to attach request event streams for all pending clients.
    bPendingClients <- accum M.empty (merge eDoAddPending eDoRemovePending)
  return bPendingClients


-- | An event which fires when a pending client should be removed.
evtRemovePending :: Behavior (M.Map Int ActivePendingClient)
                 -> Event Int
evtRemovePending bPendingClients =
    -- Map eRemoveEvt over all the pending clients and merge the resulting event
    -- streams.
    switchMerge (map eRemoveEvt <$> M.toList <$> bPendingClients)
  where
    -- Creates a remove event stream for a single client.
    eRemoveEvt :: (Int, ActivePendingClient) -> Event Int
    eRemoveEvt (cid, client) = once (const cid <$> apcAttach client)


-- | Creates an event that fires when a new pending client shows up.
evtNewPending :: AcidState ChatCoreState
              -> Event PendingClient
              -> Reactive (Event (Int, ActivePendingClient))
evtNewPending acid eNewPending =
    -- Set up new pending clients and tag them with ID numbers.
    tagIds $ execute (pendingClient acid <$> eNewPending)



--------------------------------------------------------------------------------
-- Users
--------------------------------------------------------------------------------


behUsers :: AcidState ChatCoreState
         -> Event ChatCoreUser -- ^ Event to fire to add a new user.
         -> Behavior (M.Map Int ActivePendingClient)
         -> Reactive (Behavior [ChatUser], IO ())
behUsers acid eNewUser bPendingClients = do
    let eDoAddUser = (:) <$> execute (initUser <$> eNewUser)
    bUsers <- accum [] eDoAddUser
    let clean = do
          usrs <- sync $ sample bUsers
          mapM_ cleanupChatUser usrs
    return (bUsers, clean)
  where
    initUser usrSt =
        chatUser usrSt acid $ evtUserClientAttach uName bPendingClients
      where
        uName = view usrStName usrSt

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- | The core of Chat Core.
core :: AcidState ChatCoreState -> IO ()
core acid = do
  (eNewUser, pushNewUser) <- sync newEvent
  connLs <- sequence connListeners
  cleanup <- sync $ do
    rec
      bPendingClients <- behPendingClients acid connLs
      (_, clean) <- behUsers acid eNewUser bPendingClients
      
    return clean

  -- Push an add user event for all the users.
  users <- getUsers acid
  mapM_ (sync . pushNewUser) users
  -- The main thread becomes useless at this point.
  _ <- forever $ threadDelay (1000 * 1000)
  cleanup
  return ()

runCore :: IO ()
runCore = withLocalState initialChatCoreState core



--------------------------------------------------------------------------------
-- Running Pending Clients
--------------------------------------------------------------------------------

-- | Represents an active pending client.
data ActivePendingClient = ActivePendingClient
    { apcAttach :: Event (ChatUserName, RemoteClient) -- ^ Fires when the client attaches.
    , apcRemove :: Event () -- ^ Fires when the client should be removed.
    , apcAuthed :: Behavior Bool
    }


-- | Maps a `PendingClientInfo` event stream to an event stream of events that
-- fire when a client should be attached to a user.
pendingClient :: AcidState ChatCoreState
              -> PendingClient
              -> Reactive ActivePendingClient
pendingClient acid client = do
  rec
    let clientInfo = client $ PendingClientCtx bAuthed

    let checkAuth :: (ChatUserName, Password) -> IO (Maybe ChatUserName)
        checkAuth (uname, passwd) = do
            result <- authClient acid (uname, passwd)
            return $ if result then Just uname else Nothing
  
    let eAuthResult :: Event (Maybe ChatUserName)
        eAuthResult = executeAsyncIO (checkAuth <$> pcRequestAuth clientInfo)
    let eAttach = once $ pcAttachUser clientInfo

    bAuthedUser <- hold "" $ filterJust eAuthResult
    let bAuthed = not <$> T.null <$> bAuthedUser
  
  return ActivePendingClient
         { apcAttach = snapshot (flip (,)) eAttach bAuthedUser
         , apcRemove = void eAttach
         , apcAuthed = bAuthed
         }


-- | Verifies the given authentication request for the given client.
authClient :: AcidState ChatCoreState -> (ChatUserName, Password) -> IO Bool
authClient acid (uname, passwd) = do
    userM <- getUser acid uname
    return True
    -- return $ maybe False (verifyPassword passwd . view usrStPassword) userM
