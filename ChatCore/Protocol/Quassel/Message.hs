-- | Module for implementing Quassel protocol message stuff.
-- A lot of the data structures in this module are direct ports from the real
-- Quassel core's protocol.h header file.
--
-- https://github.com/quassel/quassel/blob/master/src/common/protocol.h
module ChatCore.Protocol.Quassel.Message where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Typeable
import Data.Int
import Data.Word
import System.IO

import ChatCore.Protocol.Quassel.QVariant
import ChatCore.Protocol.Quassel.Types


-- {{{ Data Structures

data ProtocolType = LegacyProtocol | DataStreamProtocol deriving (Show, Eq)

data ProtocolFeats = PColEncryption | PColCompression deriving (Show, Eq)


data MsgHandler = SignalProxy | HandshakeMsg


-- {{{ Handshake Messages

data HandshakeMsg
    = RegisterClientMsg
        { rcClientVsn   :: T.Text
        , rcBuildDate   :: T.Text
        }
    | ClientDeniedMsg T.Text
    | ClientRegisteredMsg
        { crFeatures        :: Word32
        }

    | LoginMsg
        { lmUser        :: T.Text
        , lmPassword    :: T.Text
        }
    | LoginFailMsg T.Text
    | LoginSuccessMsg
    | SessionStateMsg
        { ssIdents  :: T.Text
        , ssBufs    :: [BufferInfo]
        , ssNets    :: [T.Text]
        }
    deriving (Show, Eq)

-- }}}

-- {{{ Signal Proxy Messages

data SignalProxyMsg
    -- Not sure what this message is for yet.
    = SyncMsg
        { smClassName   :: T.Text
        , smObjectName  :: T.Text
        , smSlotName    :: T.Text
        , smParams      :: [QVariant]
        }
    -- Calls the given remote slot.
    | RpcCallMsg
        { rcSlotName    :: T.Text
        , rcParams      :: [QVariant]
        }
    -- Not sure what this message is for yet.
    | InitRequestMsg
        { irClassName   :: T.Text
        , irObjectName  :: T.Text
        }
    -- Not sure what this message is for yet.
    | InitDataMsg
        { idClassName   :: T.Text
        , idObjectName  :: T.Text
        , idInitData    :: [(QVariant, QVariant)]
        }
    | HeartBeatMsg UTCTime
    | HeartBeatReplyMsg UTCTime

-- }}}

-- }}}

-- {{{ Reading / Writing Messages

-- | The maximum size of a message.
maxMessageSize :: Word32
maxMessageSize = 64 * 1024 * 1024;

type NetworkSrc = ResumableSource IO B.ByteString

-- | Data structure which holds information about a remote peer's connection.
data PeerConnection = PeerConnection
    { pcHandle  :: Handle
    , pcSource  :: NetworkSrc
    }

-- {{{ Handshake Messages

-- | Gets a HandshakeMsg
getHandshakeMsg :: Get HandshakeMsg
getHandshakeMsg = do
    mmList <- getQV
    let mmap = pairElements mmList
    let mml key = fromJust $ lookup (QVByteArray key) mmap
        msgType = fromQVStr $ mml "MsgType"
    case msgType of
         "ClientInit" -> return $ RegisterClientMsg
                (fromQVStr $ mml "ClientVersion")
                (fromQVStr $ mml "ClientDate")
         "ClientInitReject" -> return $ ClientDeniedMsg
                (fromQVStr $ mml "Error")
         "ClientInitAck" -> return $ ClientRegisteredMsg
                (fromQVUint $ mml "CoreFeatures")

         "ClientLogin" -> return $ LoginMsg
                (fromQVStr $ mml "User")
                (fromQVStr $ mml "Password")
         "ClientLoginReject" -> return $ LoginFailMsg
                (fromQVStr $ mml "Error")
         "ClientLoginAck" -> return LoginSuccessMsg
         -- SessionInit not implemented because it is not sent by the client.
         -- The above Ack or Reject messages aren't either, but they're simple
         -- enough to implement, so I did it anyway.


-- | Converts the given HandshakeMsg to a QVariant map.
hsMsgToMap :: HandshakeMsg -> [(QVariant, QVariant)]
hsMsgToMap = keysToQV . mtm
  where
    keysToQV = map (\(k, v) -> (QVByteArray k, v))
    qStr = QVString . QString
    mtm (ClientDeniedMsg err) =
        [ ("MsgType", qStr "ClientInitReject")
        , ("Error", qStr err)
        ]
    mtm (ClientRegisteredMsg feats) =
        [ ("MsgType", QVByteArray "ClientInitAck")
        , ("CoreFeatures", QVUint feats)
        , ("Configured", QVBool True)
        , ("LoginEnabled", QVBool True)
        ]
    mtm (LoginFailMsg err) =
        [ ("MsgType", qStr "ClientLoginReject")
        , ("Error", qStr err)
        ]
    mtm (LoginSuccessMsg) =
        [ ("MsgType", qStr "ClientLoginAck") ]


-- | Writes a HandshakeMsg
putHandshakeMsg :: HandshakeMsg -> Put
putHandshakeMsg msg =
    putQV mmlist
  where
    mmlist = unpairElements mmap
    mmap = hsMsgToMap msg

-- }}}

-- {{{ Signal Proxy Messages

-- | Gets a SignalProxyMsg
getSigProxyMsg :: Get SignalProxyMsg
getSigProxyMsg = do
    -- Read the parameter list.
    p <- getQV
    return $ case p of
        -- Sync
        (QVUint 1:qvClass:qvObj:qvSlot:params) ->
            SyncMsg
                (fromQVText qvClass)
                (fromQVText qvObj)
                (fromQVText qvSlot)
                params
        -- RpcCall
        (QVUint 2:qvSlot:params) ->
            RpcCallMsg
                (fromQVText qvSlot)
                params
        -- InitRequest
        (QVUint 3:qvClass:qvObject:[]) ->
            InitRequestMsg
                (fromQVText qvClass)
                (fromQVText qvObject)
        -- InitData
        (QVUint 4:qvClass:qvObject:params) ->
            InitDataMsg
                (fromQVText qvClass)
                (fromQVText qvObject)
                (pairElements params)
        -- HeartBeat
        (QVUint 5:qvTime:[]) ->
            HeartBeatMsg (fromQVTime qvTime)
        -- HeartBeatReply
        (QVUint 6:qvTime:[]) ->
            HeartBeatReplyMsg (fromQVTime qvTime)


-- }}}

-- | Reads a message bytestring.
getMessageBytes :: Get B.ByteString
getMessageBytes = do
    -- Check the size of the message.
    size <- getWord32be
    if size == 0
       then fail "Peer sent an empty message."
       else return ()
    if size > maxMessageSize
       then fail ("Peer sent a message that was too large. (" ++ show size ++ " bytes)")
       else return ()
    -- Read the message.
    getByteString $ fromIntegral size


-- }}}

-- {{{ Utility Functions

pairElements :: [a] -> [(a, a)]
pairElements (x:x':xs) = (x,x'):pairElements xs
pairElements [] = []

unpairElements :: [(a, a)] -> [a]
unpairElements ((x, x'):xs) = x:x':unpairElements xs
unpairElements [] = []

-- }}}

