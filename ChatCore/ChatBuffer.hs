module ChatCore.ChatBuffer where

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
    , cleanupChatBuffer :: IO ()
    } deriving (Typeable)
$(makeLenses ''ChatBuffer)


-- | A chat buffer with the given input and output events.
chatBuffer :: ChatUserName -> ChatNetworkName -> ChatBufferName
           -> Event IRCLine -- ^ Buffer-related IRC messages.
           -> Reactive ChatBuffer
chatBuffer uName netName bufName eRecvLine = do
  rec
    -- Function which creates an event which receives IRC lines with the given command.
    let eRecvCmd :: IRCCommand -> Event IRCLine
        eRecvCmd = filterCmd eRecvLine

    let eBufEvent :: Event BufferEvent
        eBufEvent = eLineBufEvent
    
    -- Buffer events derived directly from eRecvLine.
    let eLineBufEvent = filterJust (bufEventForLine <$> eRecvLine)
    
    
    --------------------------------------------------------------------------------
    -- Users
    --------------------------------------------------------------------------------

    -- Behavior with a list of users in the buffer.
    bUsers <- accum [] (eAddUser <> eRemoveUser <> eClearUsers <> eAddUsers)
    
    let eJoin = UserJoin <$> filterJust (preview _UserSource
                                         <$> filterSource (eRecvCmd ICmdJoin))

        ePart = filterJust $ flip fmap (eRecvCmd ICmdPart) $
                \line -> UserPart
                         <$> (line ^? ilSource . _Just . _UserSource)
                         <*> pure (line ^. ilBody)

    let eAddUser = (:) <$> _iuNick <$> joiningUser <$> eJoin
        eRemoveUser = (\nick -> filter (/=nick))
                      <$> _iuNick <$> partingUser <$> ePart
        eClearUsers = const <$> eNamesBegin
        eAddUsers = (++) <$> eNamesContinued

    -------- NAMES List --------
    -- True if we're receiving names. If this is False and we get a RPL_NAMES
    -- message, the user list is cleared. This is set to false when the names
    -- list ends.
    bReceivingNames <- hold False ((const True <$> eNamesBegin) <> 
                                   (const False <$> eNamesEnd))

    -- TODO: Handle @ and ! prefixes for ops and voiced people.
    let eNamesList = filterJust (fmap T.words
                                 <$> preview (ilBody . _Just)
                                 <$> eRecvCmd (ICmdOther "353"))
        eNamesBegin = gate eNamesList (not <$> bReceivingNames)
        eNamesContinued = gate eNamesList bReceivingNames
        eNamesEnd = void $ eRecvCmd $ ICmdOther "366"
    
    
    --------------------------------------------------------------------------------
    -- Logging
    --------------------------------------------------------------------------------

    -- The chat log for this buffer.
    let bufLog = mkBufferLog defLogPath $ BufferLogId uName netName bufName

  -- Write buffer events to the log.
  cleanupLogListen <- listen eBufEvent $
                      \evt -> do
                        now <- getCurrentTime
                        writeBufferLog bufLog $ BufLogLine now evt

  return $ ChatBuffer bufName bufLog eBufEvent bUsers cleanupLogListen




bufEventForLine :: IRCLine -> Maybe BufferEvent
bufEventForLine (IRCLine (Just source) ICmdPrivmsg [_] (Just msg)) =
    Just $ UserMessage source msg
bufEventForLine (IRCLine (Just source) ICmdNotice [_] (Just msg)) =
    Just $ NoticeMessage source msg
bufEventForLine _ = Nothing


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
