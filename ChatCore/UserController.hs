-- | The user controller contains and manages all of the network controllers
-- and client connections for a user. It is responsible for routing events
-- between these objects.
module ChatCore.UserController where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL

import ChatCore.NetworkController
import ChatCore.Protocol
import ChatCore.Types
import ChatCore.Events

data UserState = UserState
    { usNetworks    :: S.Seq NetworkState   -- The networks the user is connected to.
    , usClients     :: [ClientConnection]   -- The clients connected as this user.
    }

-- State monad for the user.
type UserHandler = StateT UserState IO

-- | Executes a user handler monad.
execUserHandler :: UserHandler a -> UserState -> IO UserState
execUserHandler = execStateT

-- | A consumer which handles a stream of events.
eventHandler :: UserState -> Consumer ClientEvent IO UserState
eventHandler = CL.foldM $ handler
  where
    handler state evt = execUserHandler (handleClientEvent $ evt) state

-- | Handles a client event.
handleClientEvent :: ClientEvent -> UserHandler ()
handleClientEvent evt@(SendMessage netId dest msg) =
    withNetHandler netId $ handleSendMessage dest msg

withNetHandler :: ChatNetworkId -> NetworkHandler () -> UserHandler ()
withNetHandler cnId func = withNetwork cnId $ execNetworkHandler func

-- | A user state action which takes a network ID and a function which
-- transforms a network state and uses that function to change the state of the
-- network with the given ID.
-- If no network with the given ID exists, this function will do nothing.
withNetwork :: ChatNetworkId -> (NetworkState -> IO NetworkState) -> UserHandler ()
withNetwork cnId func = do
    networks <- gets usNetworks
    let mIdx = S.findIndexL ((== cnId) . netId) networks
    case mIdx of
         Nothing -> return ()
         Just idx -> do
             newState <- lift $ func $ (flip S.index) idx networks
             modify (\s -> s { usNetworks = S.update idx newState $ usNetworks s })

