Chat Core Protocol System
=========================

This is a literate Haskell document which aims to describe how Chat Core's
multi-protocol system works.

First, the module definition and imports.

\begin{code}

{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies #-}
module ChatCore.Protocol where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Maybe
import qualified Data.IxSet as I
import qualified Data.Text as T
import FRP.Sodium

import ChatCore.ChatNetwork
import ChatCore.ChatBuffer
import ChatCore.Events
import ChatCore.Types

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
    { clNewConn :: Event PendingClient
    , clName :: T.Text -- | A human readable name for the listener.
    , clId :: T.Text -- | An ID string for the listener.
    }

\end{code}


Pending Connections
===================

When a client initiates a connection, there is an intermediate state between
the time when the client initially connects, and the time when the client
authenticates with the core and is assigned to a user controller. Connections
in this intermediate phase are called pending clients.

A pending client is implemented as a function which takes a set of events and
behaviors in a `PendingClientCtx` and returns a `PendingClientInfo`
structure. These structures provide interfaces for communication between the
pending client and the core.

\begin{code}

data PendingClientCtx = PendingClientCtx
    { pcAuthed :: Behavior Bool -- ^ True if the client has authenticated as a user.
    }

data PendingClientInfo = PendingClientInfo
    { pcRequestAuth :: Event (ChatUserName, Password)
    -- | Event to push a `RemoteClient` to attach to a user. When this event
    -- fires, the pending client will be removed.
    -- If this event fires before the client authenticates, it will not be
    -- attached and the pending client will be dropped.
    -- NOTE: Do not fire this event in the same transaction as the
    -- `pcRequestAuth` event. Doing so will not allow the client to
    -- authenticate.
    , pcAttachUser :: Event RemoteClient
    }

type PendingClient = PendingClientCtx -> IO PendingClientInfo

\end{code}


Remote Clients
==============

In the protocol system, a remote connection is represented as a `RemoteClient`.
A `RemoteClient` is a function which takes a `RemoteClientCtx` object and
returns an `IO RemoteClientInfo` action which executes the client and
returns an data structure containing information about the client.

The `RemoteClientCtx` object is a data structure holding various behaviors and
events that the client might need to react to.

\begin{code}

data RemoteClientCtx = RemoteClientCtx
    { rcUserName :: ChatUserName
    , rcCoreEvts :: Event CoreEvent
    , rcNetworks :: Behavior (I.IxSet ChatNetwork)
    }

data RemoteClientInfo = RemoteClientInfo
    { rcCommands :: Event ClientCommand
    , rcDisconnect :: Event ()
    , cleanupRemoteClient :: IO ()
    }

type RemoteClient = RemoteClientCtx -> IO RemoteClientInfo

\end{code}


Utility Functions
=================

Finally, some useful utility functions for client code to use.

\begin{code}

-- | Reactive action which gets the network with the given name.
getUserNet :: Behavior (I.IxSet ChatNetwork)
           -> ChatNetworkName
           -> Reactive (Maybe ChatNetwork)
getUserNet bNets netName =
    (I.getOne . I.getEQ netName) <$> sample bNets

-- | Reactive action which gets the buffer with the given network and buffer
-- name.
getUserBuf :: Behavior (I.IxSet ChatNetwork)
           -> ChatNetworkName -> ChatBufferName
           -> Reactive (Maybe ChatBuffer)
getUserBuf bNets netName bufName =
    runMaybeT $ do
      net <- MaybeT $ getUserNet bNets netName
      MaybeT $ getNetBuf net bufName

-- | Reactive action which gets the buffer with the given name.
getNetBuf :: ChatNetwork -> ChatBufferName -> Reactive (Maybe ChatBuffer)
getNetBuf network bufName =
    (I.getOne . I.getEQ bufName) <$> sample (network ^. bNetworkBuffers)

\end{code}
