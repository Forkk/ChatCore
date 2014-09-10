module ChatCore.Protocol.Quassel.Types where

import qualified Data.Text as T
import Data.Typeable
import Data.Int
import Data.Word

import ChatCore.Util.KeyedEnum


mkKeyedEnum "BufferType" "intToBType" "btypeToInt" Nothing
    [ ("InvalidBuffer", 0x00 :: Int16)
    , ("StatusBuffer",  0x01)
    , ("ChannelBuffer", 0x02)
    , ("QueryBuffer",   0x04)
    ]

data BufferInfo = BufferInfo
    { biId      :: Int32
    , biNetId   :: Int32
    , biType    :: BufferType
    , biGroupId :: Word32
    , biName    :: T.Text
    } deriving (Show, Eq, Typeable)


mkKeyedEnum "IrcMsgType" "intToImType" "imTypeToInt" Nothing
    [ ("IMPlain",        0x000001 :: Int32)
    , ("IMNotice",       0x000002)
    , ("IMAction",       0x000004)
    , ("IMNick",         0x000008)
    , ("IMMode",         0x000010)
    , ("IMJoin",         0x000020)
    , ("IMPart",         0x000040)
    , ("IMQuit",         0x000080)
    , ("IMKick",         0x000100)
    , ("IMKill",         0x000200)
    , ("IMServer",       0x000400)
    , ("IMInfo",         0x000800)
    , ("IMError",        0x001000)
    , ("IMDayChange",    0x002000)
    , ("IMTopic",        0x004000)
    , ("IMNetsplitJoin", 0x008000)
    , ("IMNetsplitQuit", 0x010000)
    , ("IMInvite",       0x020000)
    ]

data IrcMessage = IrcMessage
    { imId      :: Int32
    , imTime    :: Word32
    , imType    :: IrcMsgType
    , imFlags   :: Word8
    , imBufInfo :: Maybe BufferInfo
    , imSender  :: T.Text
    , imContent :: T.Text
    } deriving (Show, Eq, Typeable)

