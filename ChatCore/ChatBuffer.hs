module ChatCore.ChatBuffer
    ( ChatBuffer (..)
    , bufferName
    , chatBufLog
    , bufferEvent
    , bufferUsers
    , bufferActive

    , chatBuffer

    , isForBuffer

    , getBufLogLines
    , bufStateBehavior
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Data.IxSet as I
import Data.List
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Data.Typeable
import FRP.Sodium

import ChatCore.ChatLog
import ChatCore.Events
import ChatCore.IRC
import ChatCore.IRC.FRP
import ChatCore.State
import ChatCore.Types

data ChatUserStatus = ChatOp | ChatVoice | ChatNormal
                      deriving (Typeable, Show, Read)


data ChatBuffer = ChatBuffer
    { _bufferName :: ChatBufferName
    , _chatBufLog :: BufferLog
    -- | An event stream of this buffer's `BufferEvent`s.
    , _bufferEvent :: Event BufferEvent
    -- | A behavior containing a list of the buffer's users.
    , _bufferUsers :: Behavior [Nick]
    -- | True if the buffer is active (joined the channel).
    , _bufferActive :: Behavior Bool
    , cleanupChatBuffer :: IO ()
    } deriving (Typeable)
$(makeLenses ''ChatBuffer)



--------------------------------------------------------------------------------
-- Buffer Events
--------------------------------------------------------------------------------

-- | Creates a stream of buffer events.
evtBufferEvent :: Event IRCLine -- ^ Received IRC lines.
               -> Reactive (Event BufferEvent)
evtBufferEvent eRecvLine = 
    (return . foldr merge never) =<< sequence
    [ return $ evtLineBufEvt eRecvLine ]


-- | Buffer events derived directly from received IRC lines.
evtLineBufEvt :: Event IRCLine
              -> Event BufferEvent
evtLineBufEvt eRecvLine = filterJust (bufEventForLine <$> eRecvLine)

-- | Returns `Just BufferEvent` if the given line translates directly into a
-- buffer event.
bufEventForLine :: IRCLine -> Maybe BufferEvent
bufEventForLine (IRCLine (Just source) ICmdPrivmsg [_] (Just msg)) =
    Just $ UserMessage source msg
bufEventForLine (IRCLine (Just source) ICmdNotice [_] (Just msg)) =
    Just $ NoticeMessage source msg

bufEventForLine (IRCLine (Just (UserSource user)) ICmdJoin [_] Nothing) =
    Just $ UserJoin user
bufEventForLine (IRCLine (Just (UserSource user)) ICmdPart [_] msgM) =
    Just $ UserPart user msgM

-- TODO: Only send QUIT events for buffers the quitting user is in.
bufEventForLine (IRCLine (Just (UserSource user)) ICmdQuit _ msgM) =
    Just $ UserQuit user msgM

bufEventForLine _ = Nothing



--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

behBufActive :: Bool
             -> Behavior Nick
             -> Event IRCLine
             -> Reactive (Behavior Bool)
behBufActive bufActive bNick eRecvLine =
    hold bufActive ( (const True <$> eSelfJoin) <> (const False <$> eSelfPart))
  where
    eSelfJoin = filterFirstArg $ filterSelfSource bNick $ filterCmd eRecvLine ICmdJoin
    eSelfPart = filterFirstArg $ filterSelfSource bNick $ filterCmd eRecvLine ICmdPart



--------------------------------------------------------------------------------
-- User List
--------------------------------------------------------------------------------

behBufUsers :: Event IRCLine
            -> Reactive (Behavior [Nick])
behBufUsers eRecvLine = do
    eSetNames <- evtNewNamesList eRecvLine

    -- Behavior with a list of users in the buffer.
    accum [] (eAddUser <> eRemoveUser <> (const <$> eSetNames))
  where
    cmdSrc cmd = filterSource (filterCmd eRecvLine cmd)
    eJoin = UserJoin <$> filterJust (preview _UserSource <$> cmdSrc ICmdJoin)

    ePart = filterJust $ flip fmap (filterCmd eRecvLine ICmdPart) $
            \line -> UserPart
                     <$> (line ^? ilSource . _Just . _UserSource)
                     <*> pure (line ^. ilBody)

    eAddUser = (:) <$> _iuNick <$> joiningUser <$> eJoin
    eRemoveUser = (\nick -> filter (/=nick))
                  <$> _iuNick <$> partingUser <$> ePart


-- | Event that fires when a new NAMES list is received.
evtNewNamesList :: Event IRCLine
                -> Reactive (Event [Nick])
evtNewNamesList eRecvLine = do
  rec
    -- True if we're receiving names. If this is False and we get a RPL_NAMES
    -- message, the user list is cleared. This is set to false when the names
    -- list ends.
    bReceivingNames <- hold False ((const True <$> eNamesBegin) <> 
                                   (const False <$> eNamesEnd))

    -- TODO: Handle @ and ! prefixes for ops and voiced people.
    let eNamesList = filterJust (fmap T.words
                                 <$> preview (ilBody . _Just)
                                 <$> filterCmd eRecvLine (ICmdOther "353"))
        eNamesBegin = gate eNamesList (not <$> bReceivingNames)
        eNamesContinued = gate eNamesList bReceivingNames
        eNamesEnd = void $ filterCmd eRecvLine $ ICmdOther "366"

    let eDoStartNames = const <$> eNamesBegin
        eDoAddNames = (++) <$> eNamesContinued

    bNewNamesList <- accum [] (eDoStartNames <> eDoAddNames)
  return $ execute (const (sample bNewNamesList) <$> eNamesEnd)



--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

-- | Sets up a listener to log buffer events.
setupBufLog :: ChatUserName -> ChatNetworkName -> ChatBufferName
            -> Event BufferEvent -> Reactive (BufferLog, IO ())
setupBufLog uName netName bufName eBufEvent = do
  -- The chat log for this buffer.
  let bufLog = mkBufferLog defLogPath $ BufferLogId uName netName bufName

  -- Write buffer events to the log.
  clean <- listen eBufEvent $ \evt -> do
                        now <- getCurrentTime
                        writeBufferLog bufLog $ BufLogLine now evt
  return (bufLog, clean)



--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- | A chat buffer with the given input and output events.
chatBuffer :: ChatUserName -> ChatNetworkName -> ChatBufferName
           -> Bool -- ^ True if the buffer is active.
           -> Behavior Nick -- ^ The user's nick.
           -> Event IRCLine -- ^ Buffer-related IRC messages.
           -> Reactive ChatBuffer
chatBuffer uName netName bufName bufActive bNick eRecvLine = do
  rec
    eBufEvent <- evtBufferEvent eRecvLine
    bActive <- behBufActive bufActive bNick eRecvLine
    bUsers <- behBufUsers eRecvLine
    (bufLog, cleanup) <- setupBufLog uName netName bufName eBufEvent

  return $ ChatBuffer bufName bufLog eBufEvent bUsers bActive cleanup



-- | True if the given IRC line should be handled by the buffer with the given name.
isForBuffer :: ChatBufferName -> IRCLine -> Bool
-- PRIVMSG / NOTICE
isForBuffer bufName line@(IRCLine { _ilCommand = ICmdPrivmsg }) =
    line ^? (ilArgs . _head) == Just bufName
isForBuffer bufName line@(IRCLine { _ilCommand = ICmdNotice }) =
    line ^? (ilArgs . _head) == Just bufName

-- JOIN / PART
isForBuffer bufName line@(IRCLine { _ilCommand = ICmdJoin }) =
    line ^? (ilArgs . _head) == Just bufName
isForBuffer bufName line@(IRCLine { _ilCommand = ICmdPart }) =
    line ^? (ilArgs . _head) == Just bufName

-- NAMES list
isForBuffer bufName line@(IRCLine { _ilCommand = ICmdOther "353" }) =
    line ^? (ilArgs . ix 2) == Just bufName
isForBuffer bufName line@(IRCLine { _ilCommand = ICmdOther "366" }) =
    line ^? (ilArgs . ix 1) == Just bufName

-- NICK and QUIT are always handled. The buffer will determine whether they are
-- relevant based on whether the user is in the channel.
isForBuffer _ (IRCLine { _ilCommand = ICmdQuit }) = True
isForBuffer _ (IRCLine { _ilCommand = ICmdNick }) = True

isForBuffer _ _ = False


--------------------------------------------------------------------------------
-- API Functions
--------------------------------------------------------------------------------

getBufLogLines :: ChatBuffer -> Integer -> UTCTime -> IO [ChatLogLine]
getBufLogLines buf lineCount startTime =
    genericTake lineCount <$> readBufferLog (buf ^. chatBufLog) startTime


-- | Turns a `ChatBuffer` into a `Behavior ChatCoreBuffer` for storing in the
-- database.
bufStateBehavior :: ChatBuffer -> Behavior ChatCoreBuffer
bufStateBehavior buf = ChatCoreChannelBuffer
                       <$> pure (view bufferName buf)
                       <*> view bufferActive buf


--------------------------------------------------------------------------------
-- Indexable
--------------------------------------------------------------------------------

instance I.Indexable ChatBuffer where
    empty = I.ixSet [ I.ixFun ((:[]) . view bufferName) ]

-- Eq and Ord so we can use IxSet.
instance Eq ChatBuffer where
    a == b = bufName a == bufName b
      where
        bufName = view bufferName

instance Ord ChatBuffer where
    compare a b = bufName a `compare` bufName b
      where
        bufName = view bufferName
