Chat Core Protocol System
=========================

This is a literate Haskell document which aims to describe how Chat Core's
multi-protocol system works.

First, the module definition and imports.

\begin{code}

{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies #-}
module ChatCore.Protocol where

import Control.Applicative
import Control.Concurrent.Actor
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Text as T
import FRP.Sodium

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
    , rcNetworks :: Behavior [ChatNetworkInfo]
    }

data RemoteClientInfo = RemoteClientInfo
    { rcCommands :: Event ClientCommand
    }

type RemoteClient = RemoteClientCtx -> IO RemoteClientInfo

\end{code}
