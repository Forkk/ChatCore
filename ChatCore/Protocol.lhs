Chat Core Protocol System
=========================

This is a literate Haskell document which aims to describe how Chat Core's
multi-protocol system works.

First, the module definition and imports.

\begin{code}

{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies #-}
module ChatCore.Protocol
    (
    -- * Connection Listener
      ConnListener (..)
    -- * Pending Connection
    , PendingConn (..)
    -- * Remote Client
    , RemoteClient
    , RemoteClientMsg (..)
    , RemoteClientHandle
    , execRemoteClient
    , receivedClientCmd
    , nextCoreEvent
    , coreEventSrc
    , coreEvtOr
    -- * Communicating With Clients
    , rcSendCoreEvt
    ) where

import Control.Applicative
import Control.Concurrent.Actor
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.TMChan
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.Typeable

import ChatCore.Events
import ChatCore.Types
import {-# SOURCE #-} ChatCore.CoreController
import {-# SOURCE #-} ChatCore.UserController

\end{code}


Protocol Definitions
====================

When implementing a protocol, the first thing to do is write some code to
listen for connections. For this, Chat Core provides connection listeners.

A connection listener is an object of the `ConnListener` type. It is a simple
data structure containing a function which will listen for clients connecting
with a certain protocol.

\begin{code}

data ConnListener = ConnListener
    { listenerFunc :: Source IO PendingConn -- | The function to listen for events.
    , clName :: T.Text -- | A human readable name for the listener.
    , clId :: T.Text -- | An ID string for the listener.
    }

\end{code}


Pending Connections
===================

When a client initiates a connection, there is an intermediate state between
the time when the client initially connects, and the time when the client
authenticates with the core and is assigned to a user controller. Connections
in this intermediate phase are called pending connections and they are simply
implemented as IO actions which take a core controller handle and return a
tuple containing the connection's user ID and a `RemoteClient` action which
will run the connection.

The core controller handle that is passed to the pending connection object can
be used to communicate with the core controller to do things such as user
authentication.

\begin{code}

type PendingConn = CoreCtlHandle -> IO (UserId, RemoteClient ())

\end{code}


Remote Clients
==============

A core component of the protocol system is the client connection object. The
client connection object represents a client that has connected to the server
and authenticated as a user.

The interface between the core systems and the client connection object
consists of a single function which takes a user controller's handle as an
argument and returns a `RemoteClient` monad that executes the client.

\begin{code}

-- TODO: Find a way to communicate with the user controller without breaking
-- abstraction.

data RemoteClientMsg = RCCoreEventMsg CoreEvent
instance ActorMessage RemoteClientMsg

newtype RemoteClient a =
    RC { unRC :: (ReaderT (ActorHandle UserCtlActorMsg) (ActorM RemoteClientMsg) a) }
    deriving (Monad, MonadIO, MonadThrow, Applicative, Functor)

type RemoteClientHandle = ActorHandle RemoteClientMsg

fromRCMsg :: RemoteClientMsg -> CoreEvent
fromRCMsg (RCCoreEventMsg evt) = evt


rcSendCoreEvt :: (MonadIO m) => RemoteClientHandle -> CoreEvent -> m ()
rcSendCoreEvt rc = send rc . RCCoreEventMsg


instance MonadBase IO RemoteClient where
    liftBase = RC . liftBase

instance MonadBaseControl IO RemoteClient where
    newtype StM RemoteClient a =
        StMRC { unStMRC :: StM (ReaderT UserCtlHandle (ActorM RemoteClientMsg)) a }
    liftBaseWith f =
        RC . liftBaseWith $ \runInBase -> f $ liftM StMRC . runInBase . unRC
    restoreM = RC . restoreM . unStMRC


-- | Executes the given `RemoteClient` monad as an actor with the given user
-- control handle.
execRemoteClient :: UserCtlHandle -> RemoteClient a -> ActorM RemoteClientMsg a
execRemoteClient ucHandle (RC actorM) =
    runReaderT actorM ucHandle

-- | Sends the given `ClientCommand` to the user controller.
receivedClientCmd :: ClientCommand -> RemoteClient ()
receivedClientCmd cmd = RC $ do
    usrCtl <- ask
    lift $ ucSendClientCmd usrCtl cmd

-- | Waits for the next core event.
nextCoreEvent :: RemoteClient CoreEvent
nextCoreEvent = RC $ (fromRCMsg <$> lift receive)

-- | Conduit source for core events.
coreEventSrc :: Source RemoteClient CoreEvent
coreEventSrc = (yield =<< lift nextCoreEvent) >> coreEventSrc

\end{code}

Remote client controllers should call the `nextCoreEvent` function to get
events. This function will block, so it is advisable to use the async library
to wait on the `nextCoreEvent` function and the IO handle at the same time.
Alternatively, use the utility functions described below.


Most remote client protocol implementations will probably follow quite similar
patterns. For example, wait for IO on a handle or a core event and then do some
monadic action. Some useful utility functions are provided to facilitate that.

\begin{code}

-- | Wait for the given IO action or a core event.
-- FIXME: There is a possible race condition here where the IO action might
-- be cancelled in the middle of an operation that cannot be rolled back if
-- a core event occurs right before the IO action finishes.
coreEvtOr :: IO a -> RemoteClient (Either CoreEvent a)
coreEvtOr func = runResourceT $ do
    -- Run the given IO action on an async thread.
    (_, funcA) <- allocate (async func) cancel
    -- Get an STM action to receive messages.
    stmRcv <- lift $ RC $ lift $ receiveSTM
    -- Wait on the IO action to complete or a core event to be received.
    liftIO $ atomically $
        (Right <$> waitSTM funcA)
         `orElse`
        (Left  <$> fromRCMsg <$> stmRcv)

\end{code}

