Chat Core Protocol System
=========================

This is a literate Haskell document which aims to describe how Chat Core's
multi-protocol system works.

First, the module definition and imports.

> {-# LANGUAGE RankNTypes #-}
> module ChatCore.Protocol where
> 
> import Data.Conduit
> import qualified Data.Text as T
> 
> import ChatCore.Events


The Typeclasses
===============

The protocol system consists of two type classes, the `CoreType` class, and the
`CoreProtocol` typeclass. The purpose of these classes is to provide a
translation layer between Chat Core's own "internal protocol" of function calls
and the protocols of the outside world (e.g. IRC, QuasselCore, etc).
    
The `CoreType` typeclass represents a specific type of IRC core protocol. It
contains some information about the protocol (such as its name), function which
listens for connections.

The `CoreProtocol` typeclass implements a connection for a specific protocol.
All `CoreProtocol` instances are created and initialized by a `CoreType`'s
connection source. It is possible to have a `CoreType` which can create
multiple different `CoreProtocol` types. This can (and should) be used to
implement protocols which have wildly different protocol "types" that are only
decided upon after a handshake.


The `CoreType` Class
====================

The `CoreType` typeclass holds information about a specific type of IRC core
protocol. This includes information such as the protocol/bouncer's name. The
`CoreType` class also contains a function which should handle listening for new
connections.

Each `CoreType` instance contains a `connectionListener` function which is
responsible for listening for new connections. This function will be run on a
separate thread, and it should block until a new connection is made. Once the
connection is made, it should immediately call the `newConnection` function,
passing in a function to initialize the connection. Initialization should not
be done in the listener function's thread, as initialization may require doing
things that aren't thread-safe.

> type ConnectionSource = (CoreProtocol conn) => Source IO (IO conn)
> 
> class CoreType ctype where
>     coreTypeName :: ctype -> T.Text
>     coreTypeDesc :: ctype -> T.Text
>     connectionListener :: ctype -> ConnectionSource


The `CoreProtocol` Class
========================

The `CoreProtocol` class is where the meat of the protocol implementation lies.
It is responsible for handling all of the translation back and forth between
Chat Core's internal systems and the protocols of the outside world.

To do this, the `CoreProtocol` class provides an array of functions which
provide IO actions to send certain messages to the client; as well as a
function to provide a source of messages from the client.

Listening for Messages
----------------------

One of the goals of the `CoreProtocol` is to receive data from clients and
translate that into an event that Chat Core can understand. For example, in the
IRC core protocol, a "PRIVMSG" message from a client should be translated into
a `sendMessage` Chat Core event.

This is accomplished by the `eventListener` function, which is a function that
should listen for messages, translate them into a Chat Core event, and pass
them to the `handleEvent` function.

The return type of the `eventListener` function is an `EventSource`. This is an
opaque monadic type which handles the dirty business of getting those Chat Core
events where they need to go to be processed. Internally, it is implemented as
a conduit `Source`, which provides a source of Chat Core client events.

> type EventSource = Source IO ClientEvent


Sending Messages to the Client
------------------------------

The other task that the `CoreProtocol` must handle is translating events from
Chat Core into messages that the client can understand. Doing this is fairly
simple, the typeclass must implement a function which translates Chat Core's
core events into messages that the client can understand.


> class CoreProtocol conn where
>     -- | Reads messages from the client and provides a source of `ClientEvent`s.
>     eventListener   :: conn -> EventSource
>     -- | Takes a `CoreEvent` and sends the appropriate message to the client.
>     sendEvent       :: conn -> CoreEvent -> IO ()

