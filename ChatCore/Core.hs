-- | The core module defines data structures and actions related to the core state.
module ChatCore.Core where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Crypto.PasswordStore
import Data.Acid
import qualified Data.IxSet as I
import qualified Data.Text as T
import Data.Traversable hiding (mapM, sequence)
import qualified Data.Map as M
import FRP.Sodium
import FRP.Sodium.IO
import FRP.Sodium.Internal (listenTrans)
import Network

import ChatCore.ChatUser
import ChatCore.ChatNetwork
import ChatCore.Events
import ChatCore.Protocol
import ChatCore.Protocol.JSON
import ChatCore.State
import ChatCore.Types
import ChatCore.Util
import ChatCore.Util.FRP

connListeners :: [IO ConnListener]
connListeners =
    [ jsonConnListener (PortNumber 1337)
    ]


--------------------------------------------------------------------------------
-- Reactive
--------------------------------------------------------------------------------

-- | The core of Chat Core.
core :: AcidState ChatCoreState -> IO ()
core acid = do
  (eNewUser, pushNewUser) <- sync newEvent
  connLs <- sequence connListeners
  sync $ do
    rec
      --------------------------------------------------------------------------------
      -- Clients
      --------------------------------------------------------------------------------
      
      -- TODO: Maybe find a simpler way to implement this system. It's quite
      -- complicated as is.
      
      -- An event for new pending clients. We get this by just merging all of
      -- the connection listeners' new client events.
      let eNewConn = foldr merge never (clNewConn <$> connLs)
      
      -- This event fires after a new pending client thread is started.
      let eNewPending = executeAsyncIO (pendingClient acid <$> eNewConn)
      -- Tag pending connections with IDs.
      eNewPendingWithId' <- tagIds eNewPending
      -- Copy those IDs into the attach event streams.
      -- Event (Int, Event (Maybe (..))) -> Event (Int, Event (Int, Maybe (..)))
      let eNewPendingWithId = copyPendingIdInto <$> eNewPendingWithId'
          copyPendingIdInto :: (Int, Event (Maybe (ChatUserName, RemoteClient)))
                            -> (Int, Event (Int, Maybe (ChatUserName, RemoteClient)))
          copyPendingIdInto (pid, eAttach) = (pid, (pid, ) <$> eAttach)
      
      -- A stream of events which insert or remove pending clients in the pending
      -- client list.
      let eAddPending = uncurry M.insert <$> eNewPendingWithId
          -- Remove any client that requests attachment. To do this, we just take
          -- the first element of each attach request tuple.
          eRemovePending = M.delete <$> fst <$> eAttachRequest
      
      -- A map of IDs to attach request event streams for all pending clients.
      bAttachEventMap <- accum M.empty (merge eAddPending eRemovePending)
      
      -- eAttachRequest fires any time a pending client requests to attach to a
      -- user. The second element of the tuples it fires will be Nothing if the
      -- attach request failed. The first element indicates the ID of the pending
      -- client.
      let eAttachRequest :: Event (Int, Maybe (ChatUserName, RemoteClient))
          eAttachRequest = switchE bAttachEvent
          bAttachEvent = foldr merge never <$> bAttachEventList
          bAttachEventList = map snd <$> M.toList <$> bAttachEventMap
      
      -- An event stream of only successful attachment requests. This stream does
      -- not include the pending client's ID.
      let eAttachSuccess :: Event (ChatUserName, RemoteClient)
          eAttachSuccess = filterJust (snd <$> eAttachRequest)
      
      -- An event stream of clients to attach to the given user.
      let eUserNewClient uname = snd <$> filterE ((==uname) . fst) eAttachSuccess
      
      --------------------------------------------------------------------------------
      -- Users
      --------------------------------------------------------------------------------
      
      let initUser usrSt = chatUser usrSt acid $ eUserNewClient (usrSt ^. usrStName)
          eAddUser = (:) <$> executeAsyncIO (initUser <$> eNewUser)
      bUsers <- accum [] eAddUser
    return ()

  -- Push an add user event for all the users.
  users <- getUsers acid
  mapM_ (sync . pushNewUser) users
  -- The main thread becomes useless at this point.
  _ <- forever $ threadDelay (1000 * 1000)
  return ()

runCore :: IO ()
runCore = withLocalState initialChatCoreState core


-- | Maps a `PendingClient` event stream to an event stream of events that fire
-- when a client should be attached to a user.
pendingClient :: AcidState ChatCoreState
              -> PendingClient
              -> IO (Event (Maybe (ChatUserName, RemoteClient)))
pendingClient acid client = do
  (bAuthed, pushAuthed) <- sync $ newBehavior False

  let clientCtx = PendingClientCtx bAuthed
  clientInfo <- client clientCtx

  let checkAuth :: (ChatUserName, Password) -> IO (Maybe ChatUserName)
      checkAuth (uname, passwd) = do
          result <- authClient acid (uname, passwd)
          return $ if result then Just uname else Nothing

  let eAuthResult :: Event (Maybe ChatUserName)
      eAuthResult = executeAsyncIO (checkAuth <$> pcRequestAuth clientInfo)
  let eAttach = once $ pcAttachUser clientInfo

  bAuthedUser <- sync $ hold "" $ filterJust eAuthResult
  -- TODO: unlisten
  -- TODO: Avoid use of listenTrans. For some reason, rec causes this code to
  -- lock up. I suspect the problem could show up again, so actually fixing it
  -- would be nice.
  sync $ listenTrans (updates bAuthedUser) $ const $ pushAuthed True

  let mkRetVal rc uname = if T.null uname
                             then Nothing
                             else Just (uname, rc)
  return $ snapshot mkRetVal eAttach bAuthedUser

-- | Verifies the given authentication request for the given client.
authClient :: AcidState ChatCoreState -> (ChatUserName, Password) -> IO Bool
authClient acid (uname, passwd) = do
    userM <- getUser acid uname
    return True
    -- return $ maybe False (verifyPassword passwd . view usrStPassword) userM
