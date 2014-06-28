{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies #-}
-- | This module provides a simple ChatCore protocol that just exposes Chat
-- Core's internal event system through a simple JSON interface.
-- It's not supported by any client, but it provides a good baseline as well as
-- a good way to test Chat Core's event system.
module ChatCore.Protocol.JSON where

import Control.Monad.Trans
import Data.Conduit
import Data.Maybe
import qualified Data.Conduit.List as CL
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import Network
import System.IO

import ChatCore.Events
import ChatCore.Protocol
import ChatCore.Types

jsonCoreType :: JSONCoreType
jsonCoreType = JSONCoreType { jsonCorePort = PortNumber 1337 }

-- | Data structure for the JSON core type.
data JSONCoreType = JSONCoreType
    { jsonCorePort :: PortID -- The port to listen on.
    }

instance CoreType JSONCoreType JSONConnection where
    coreTypeName _ = "JSON Serialization"
    coreTypeDesc _ = "A protocol that simply exposes Chat Core's internal event system via JSON serialization."
    connectionListener coreType = do
        -- Wait for a connection.
        sock <- lift $ listenOn (jsonCorePort coreType)
        -- Accept the sock.
        (handle, host, port) <- lift $ accept sock
        -- Create the connection object.
        let conn = JSONConnection { jcHandle = handle
                                  , jcRemoteHost = host
                                  , jcPortNumber = port
                                  , jcSocket = sock
                                  }
        -- Give an IO action that just returns our connection object.
        -- If we wanted to do any processing, we would do so inside this IO
        -- action.
        newConnection $ return conn


-- | Data structure representing a JSON core connection.
data JSONConnection = JSONConnection
    { jcHandle     :: Handle       -- | IO handle for reading and writing.
    , jcRemoteHost :: HostName     -- | The client's hostname.
    , jcPortNumber :: PortNumber   -- | The port number.
    , jcSocket     :: Socket       -- | The socket to receive data from.
    }

instance CoreProtocol JSONConnection where
    eventListener conn =
        src $= (CL.map $ decodeStrict) $= (CL.filter $ isJust) $= (CL.map $ fromJust)
      where
        src = (yield =<< (lift $ B.hGetLine $ jcHandle conn)) >> src

    sendEvent _ _ = return ()


-- JSON Parsing
instance FromJSON ClientEvent where
    parseJSON _ = do
        return $ SendMessage (DestChan "#channel") "This is a placeholder."


