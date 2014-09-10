-- | Module for implementing QVariant bullshit.
module ChatCore.Protocol.Quassel.QVariant where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Typeable
import Data.Int
import Data.Word
import Language.Haskell.TH

import ChatCore.Protocol.Quassel.Types
import ChatCore.Protocol.Quassel.TH

-- {{{ QVariant Declaration

mkQVariant 0
    [ ("QVBool",            1,      ConT ''Bool)
    , ("QVInt",             2,      ConT ''Int32)
    , ("QVUint",            3,      ConT ''Word32)
    , ("QVLongLong",        4,      ConT ''Int64)
    , ("QVULongLong",       5,      ConT ''Word64)
    , ("QVDouble",          6,      ConT ''Double)
    , ("QVChar",            7,      ConT ''Word8)

    , ("QVMap",             8,      AppT ListT (AppT (AppT (TupleT 2) qvType) qvType))
    , ("QVList",            9,      AppT ListT qvType)
    , ("QVString",          10,     ConT $ mkName "QString")
    -- , ("QVStringList",      11,     [t|  |])
    , ("QVByteArray",       12,     ConT ''T.Text)
    , ("QVBitArray",        13,     ConT ''B.ByteString)
    , ("QVBufferInfo",      127,    ConT ''BufferInfo)
    , ("QVIrcMessage",      1337,   ConT ''IrcMessage)

    -- , ("QVDate",            14,     [t|  |])
    -- , ("QVTime",            15,     [t|  |])
    , ("QVDateTime",        16,     ConT ''UTCTime)

    -- , ("QVUrl",             17,     [t|  |])
    -- , ("QVLocale",          18,     [t|  |])

    -- , ("QVRegEx",           27,     [t|  |])
    -- , ("QVHash",            28,     [t|  |])
    -- , ("QVLastCoreType",    29,     [t|  |])
    -- , ("QVLastGuiType",     86,     [t|  |])
    -- , ("QVUserType",        127,    [t|  |])
    ]

-- }}}

-- {{{ Common Conversion functions
-- TODO: Generate these with TH.

-- | Converts either a QVString or a QVByteArray to a T.Text value.
-- Crashes if the given QVariant isn't a QVString or QVByteArray.
fromQVStr :: QVariant -> T.Text
fromQVStr (QVString (QString val)) = val
fromQVStr (QVByteArray val) = val
fromQVStr _ = error "Given QVariant is not a string or byte array."

fromQVText :: QVariant -> T.Text
fromQVText = fromQVByteArray

fromQVByteArray :: QVariant -> T.Text
fromQVByteArray (QVByteArray val) = val

fromQVTime :: QVariant -> UTCTime
fromQVTime (QVDateTime val) = val

fromQVUint :: QVariant -> Word32
fromQVUint (QVUint val) = val

-- }}}

-- {{{ QVariant Type Instances

instance IsString QVariant where
    fromString = QVByteArray . T.pack

-- }}}

-- {{{ QVariant Functions

getQVariant :: Get QVariant
getQVariant = do
    -- Read the type ID.
    typeId <- getWord32be
    -- Read the null flag.
    isNull <- (/=0) <$> getWord8
    -- Read the value.
    readQVForId typeId

putQVariant :: QVariant -> Put
putQVariant qv = do
    -- Write the type ID.
    putWord32be $ qvTypeId qv
    -- Write the null flag. Our QVariants are never null. This may need to be
    -- changed in the future.
    putWord8 1
    -- Write the value.
    putQVariantData qv

-- }}}

-- {{{ QVariantVal

-- | Represents a type that can be stored in a QVariant.
class (Typeable a) => QVariantVal a where
    -- Get the type ID for this value as a QVariant.
    toQVariant :: a -> QVariant
    -- Get the user type ID for this value.
    userTypeId :: a -> String
    userTypeId _ = ""
    -- Put monad to serialize the value.
    putQV :: a -> Put
    -- Get monad to read the value.
    getQV :: Get a

-- }}}

-- {{{ QVariantVal Instances

instance QVariantVal QVariant where
    toQVariant = id
    putQV = putQVariant
    getQV = return (QVInvalid)

-- {{{ Primitive Types (bool/int/word/double/char)

instance QVariantVal Bool where
    toQVariant = QVBool
    putQV = put
    getQV = get

instance QVariantVal Int32 where
    toQVariant = QVInt
    putQV = put
    getQV = get

instance QVariantVal Word32 where
    toQVariant = QVUint
    putQV = put
    getQV = get

instance QVariantVal Int64 where
    toQVariant = QVLongLong
    putQV = put
    getQV = get

instance QVariantVal Word64 where
    toQVariant = QVULongLong
    putQV = put
    getQV = get

instance QVariantVal Double where
    toQVariant = QVDouble
    putQV = put
    getQV = get


instance QVariantVal Char where
    toQVariant _ = QVInvalid
    putQV = putWord16be . fromIntegral . fromEnum
    getQV = toEnum <$> fromIntegral <$> getWord16be

instance QVariantVal Word8 where
    toQVariant = QVChar
    putQV = put
    getQV = get

-- }}}

-- {{{ Misc Types (time)

instance QVariantVal UTCTime where
    toQVariant = QVDateTime
    putQV t = do
        putWord32be $ fromIntegral $ toModifiedJulianDay $ utctDay t
        putWord32be $ fromIntegral $ round $ (utctDayTime t * 1000)
    getQV = do
        day <- ModifiedJulianDay <$> fromIntegral <$> getWord32be
        secs <- (/1000) <$> fromIntegral <$> getWord32be
        return $ UTCTime day secs

-- }}}

-- {{{ Collections (map/list/string/stringlist/bytestring)

-- {{{ Map

-- Special Map instance for QVariant maps.
-- This is necessary since making QVariant an instance of QVariantVal makes GHC
-- go insane.

type QVMap = M.Map QVariant QVariant

instance QVariantVal [(QVariant, QVariant)] where
    toQVariant = QVMap
    putQV m = do
        -- Write the number of elements in the map.
        putWord32be $ fromIntegral $ length m
        -- Write each pair in order.
        mapM_ putPair m
      where
        putPair (k, v) = do
            return ()
            putQVariant k
            putQVariant v
    getQV = do
        -- Read the size.
        len <- fromIntegral <$> getWord32be
        -- Read each pair.
        pairs <- mapM getPair $ replicate len ()
        return pairs
      where
        getPair _ = do
            k <- getQVariant
            v <- getQVariant
            return (k, v)

-- }}}

-- {{{ List

instance QVariantVal [QVariant] where
    toQVariant = QVList
    putQV l = do
        -- Write the size.
        putWord32be $ fromIntegral $ length l
        -- Write each element.
        mapM_ putQVariant l
    getQV = do
        -- Read the size.
        len <- fromIntegral <$> getWord32be
        mapM (\_ -> getQVariant) $ replicate len ()

-- }}}

-- {{{ String and Byte Array

newtype QString = QString T.Text deriving (Show, Read, Eq, Ord, Typeable)

instance QVariantVal QString where
    toQVariant = QVString
    putQV (QString str) = putBS $ T.encodeUtf16BE str
    getQV = QString <$> T.decodeUtf16BE <$> getBS

instance QVariantVal T.Text where
    toQVariant = QVByteArray
    putQV str = putBS $ T.encodeUtf8 str
    getQV = T.decodeUtf8 <$> getBS

instance QVariantVal B.ByteString where
    toQVariant = QVBitArray
    putQV = putBS
    getQV = getBS

putBS :: B.ByteString -> Put
putBS bs = do
    if B.length bs == 0
       then putWord32be 0xFFFFFFFF
       else do
           putWord32be $ fromIntegral $ B.length bs
           putByteString bs

getBS :: Get B.ByteString
getBS = do
    -- If the size is 0xFFFFFFFF, the string is empty.
    -- I have no idea why Qt does this, but it seems pretty dumb.
    len <- fromIntegral <$> getWord32be
    if len == 0xFFFFFFFF
       then return B.empty
       else getByteString len

-- }}}

-- {{{ Custom Types (BufferInfo/IrcMessage)

instance QVariantVal BufferInfo where
    toQVariant = QVBufferInfo
    putQV bi = do
        putInt32be $ biId bi
        putInt32be $ biNetId bi
        putInt16be $ btypeToInt $ biType bi
        putWord32be $ biGroupId bi
        putQV $ biName bi
    getQV = BufferInfo
        <$> getInt32be -- Buffer ID
        <*> getInt32be -- Network ID
        <*> (intToBType <$> getInt16be) -- Type ID
        <*> getWord32be -- Group ID.
        <*> getQV

instance QVariantVal IrcMessage where
    toQVariant = QVIrcMessage
    putQV im = do
        putInt32be $ imId im
        putWord32be $ imTime im
        putWord32be $ imTypeToInt $ imType im
        putWord8 $ imFlags im
        putQV $ fromJust $ imBufInfo im
        putQV $ imSender im
        putQV $ imContent im
    getQV = IrcMessage
        <$> getInt32be
        <*> getWord32be
        <*> (intToImType <$> getWord32be)
        <*> getWord8
        <*> pure Nothing
        <*> getQV
        <*> getQV

-- }}}

-- }}}

-- {{{ Utility Functions

getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be

getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be

getInt64be :: Get Int64
getInt64be = fromIntegral <$> getWord64be


putInt16be :: Int16 -> Put
putInt16be = putWord16be . fromIntegral

putInt32be :: Int32 -> Put
putInt32be = putWord32be . fromIntegral

putInt64be :: Int64 -> Put
putInt64be = putWord64be . fromIntegral

-- }}}

