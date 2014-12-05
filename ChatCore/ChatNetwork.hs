-- | Network logic module.
module ChatCore.ChatNetwork where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Acid
import qualified Data.IxSet as I
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Typeable
import FRP.Sodium
import FRP.Sodium.IO
import Network

import ChatCore.ChatBuffer
import ChatCore.Events
import ChatCore.IRC
import ChatCore.IRC.FRP
import ChatCore.State
import ChatCore.Types
import ChatCore.Util.FRP


-- | The @ChatNetwork@ object holds the state information for a particular IRC network.
-- It is composed of multiple objects which are joined together by lenses.
-- NOTE: The `Eq` and `Ord` instances for @ChatNetwork@ only compare the network name.
data ChatNetwork = ChatNetwork
    { _chatNetworkName :: ChatNetworkName
    , _bNetworkStatus :: Behavior ConnectionStatus
    , _bNetworkUserNick :: Behavior Nick
    , _bNetworkBuffers :: Behavior (I.IxSet ChatBuffer)
    , _eNetworkCoreEvt :: Event CoreEvent
    , cleanupChatNetwork :: IO ()
    } deriving (Typeable)
$(makeLenses ''ChatNetwork)



--------------------------------------------------------------------------------
-- Buffers
--------------------------------------------------------------------------------

-- | Creates a stream of `CoreEvent`s tagged with the buffer event information
-- for each buffer in the given behavior.
evtBufferEvent :: ChatNetworkName
               -> Behavior (I.IxSet ChatBuffer)
               -> Event CoreEvent
evtBufferEvent netName bBuffers =
    switchMerge (map (tagBufEvents netName) <$> I.toList <$> bBuffers)

-- Tags events in the given buffer event stream as core events with
-- the given buffer and the current network name.
tagBufEvents :: ChatNetworkName -> ChatBuffer -> Event CoreEvent
tagBufEvents netName buf = ChatBufferEvent netName bufName <$> eBufEvt
  where
    bufName = view bufferName buf
    eBufEvt = view bufferEvent buf


behBuffers :: ChatUserName
           -> ChatCoreNetwork
           -> Event () -- ^ Init event.
           -> Behavior Nick -- ^ The user's nick.
           -> Event IRCLine -- ^ IRC Line receive.
           -> Reactive (Behavior (I.IxSet ChatBuffer), IO ())
behBuffers uName network eInit bUserNick eRecvLine = do
  rec
    -- Find join events involving us and use them to add buffers.
    let eSelfJoin = filterFirstArg
                    $ filterSelfSource bUserNick
                    $ filterCmd eRecvLine ICmdJoin
    let eReqBuffer = eSelfJoin

    eDoAddBuf <- evtDoAddBuf uName network eInit bBuffers
                             bUserNick eRecvLine eReqBuffer
    bBuffers <- accum I.empty eDoAddBuf
  return (bBuffers, return ())


-- | Creates a stream of functions which add the appropriate entries to the
-- buffer list.
evtDoAddBuf :: ChatUserName -> ChatCoreNetwork
            -> Event () -- ^ Init event for adding initial buffers.
            -> Behavior (I.IxSet ChatBuffer) -- ^ The buffer list.
            -> Behavior Nick
            -> Event IRCLine
            -> Event ChatBufferName -- ^ Event fired when a given buffer is
                                    -- needed. Creates the buffer if it doesn't
                                    -- exist.
            -> Reactive (Event (I.IxSet ChatBuffer -> I.IxSet ChatBuffer))
evtDoAddBuf uName network eInit bBuffers bUserNick eRecvLine eReqBuf =
    return (addBuf <$> execute (mkBuffer <$> eAddBuf))
  where
    -- TODO: Maybe refactor this part further. It's still quite large.
    eNewBuf = evtCreateBufs eReqBuf bBuffers
    eInitBuf = split (const (I.toList $ view netStBuffers network) <$> eInit)

    eAddBuf :: Event ChatCoreBuffer
    eAddBuf = eNewBuf <> eInitBuf

    addBuf :: ChatBuffer -> I.IxSet ChatBuffer -> I.IxSet ChatBuffer
    addBuf buf = I.updateIx (buf ^. bufferName) buf

    -- Initializes a `ChatBuffer` object for the given buffer.
    mkBuffer :: ChatCoreBuffer -> Reactive ChatBuffer
    mkBuffer buf = chatBuffer uName netName bufName
                              (_ccBufferActive buf)
                              bUserNick
                              (filterE (isForBuffer bufName) eRecvLine)
      where
        bufName = buf ^. ccBufferName
        netName = network ^. netStName

-- | Event stream of buffer names to create new buffer objects for.
-- These are only buffers that don't exist in the database. Those that already
-- do are assumed to already be loaded.
evtCreateBufs :: Event ChatBufferName
              -> Behavior (I.IxSet ChatBuffer)
              -> Event ChatCoreBuffer
evtCreateBufs eReqBuf bBuffers = newBuf <$> snapFilterE notExist eReqBuf bBuffers
  where
    notExist reqBufName bufs = not $ I.null $ I.getEQ reqBufName bufs
    newBuf bufName = ChatCoreChannelBuffer bufName True



--------------------------------------------------------------------------------
-- IRC Connection
--------------------------------------------------------------------------------

-- | Manages the IRC connection.
behIrcConn :: Event IRCLine -- ^ Event stream of IRC lines to send.
           -> Behavior ChatCoreNetServer -- ^ Behavior of the server to connect to.
           -> Reactive ( Behavior ConnectionStatus
                       , Event IRCLine 
                       , IO () )
behIrcConn eSendLine bIrcServer = do
    -- New connection events are generated by executing `serverConn` every time
    -- the `bIrcServer` behavior changes.
    let eNewConnection = executeAsyncIO (serverConn eSendLine
                                         <$> value bIrcServer)

    -- TODO: Close the old connection when a new one is opened.
    bIrcConn <- hold nullConn eNewConnection

    -- Connection status just pulls from the IRC connection object.
    -- TODO: Reconnect automatically.
    bConnStatus <- switch (ircConnStatus <$> bIrcConn)

    -- The receive line event just takes the curren't connection's receive line event.
    let eRecvLine = switchE (ircRecvLine <$> bIrcConn)

    return (bConnStatus, eRecvLine, return ())

-- TODO: Pull from the database and cycle through servers.
behIrcServer :: Behavior ChatCoreNetServer
behIrcServer = pure $ ChatCoreNetServer "irc.esper.net" 6667

-- | Creates an IRC connection for the given server info.
serverConn :: Event IRCLine -- ^ Event stream of IRC lines to send.
           -> ChatCoreNetServer
           -> IO IRCConnection
serverConn eSendLine =
    connectIRC <$> (T.unpack . view serverHost)
               <*> (PortNumber . fromIntegral . view serverPort)
               <*> pure eSendLine


--------------------------------------------------------------------------------
-- IRC State
--------------------------------------------------------------------------------

behNick :: Event IRCLine
        -> Reactive (Behavior Nick)
behNick eRecvLine = do
  rec
    -- TODO: Fix this. Should check if it's our nick that's changing.
    let eNickChange = view (ilArgs . _head) <$> filterCmd eRecvLine ICmdNick
    -- The initial nick on connect is specified in the first argument of RPL_WELCOME.
    let eInitialNick = view (ilArgs ._head) <$> filterCmd eRecvLine (ICmdOther "001")
  hold "" (eNickChange <> eInitialNick)



--------------------------------------------------------------------------------
-- Send Lines
--------------------------------------------------------------------------------

-- | Event stream of IRC lines to send.
evtSendLine :: ChatCoreNetwork
            -> Event () -- ^ Event fires when IRC socket connects.
            -> Event () -- ^ Event fires when IRC connection is complete.
            -> Event IRCLine -- ^ Event for received lines.
            -> Event ClientCommand -- ^ Event for client commands.
            -> Behavior (I.IxSet ChatBuffer)
            -> Reactive (Event IRCLine)
evtSendLine network eConnected eConnInit eRecvLine eClientCmd bBuffers =
    (return . foldr merge never) =<< sequence
    [ evtSendInit network eConnected
    , evtSendJoinInitialChans eConnInit bBuffers
    , evtSendClientLines eClientCmd
    , return $ evtSendPong eRecvLine
    ]


-- | Initializes the IRC connection after the socket connects.
evtSendInit :: ChatCoreNetwork
            -> Event () -- ^ Event fires when IRC socket connects.
            -> Reactive (Event IRCLine)
evtSendInit _ eConnected = return $
    split (const [ userLine "ChatCore" "Chat Core"
                 , nickLine "ChatCore"
                 ] <$> eConnected)


-- | Event stream of IRC lines derived from client commands.
evtSendClientLines :: Event ClientCommand -> Reactive (Event IRCLine)
evtSendClientLines eClientCmd = return eClientCmdLines
  where
    eClientCmdLines = filterJust (lineForCmd <$> eClientCmd)
    lineForCmd (SendMessage _ dest content) = Just $ privMsgLine dest content
    lineForCmd (JoinChannel _ chan) = Just $ joinLine chan
    lineForCmd (PartChannel _ chan msgM) = Just $ partLine chan (fromMaybe "" msgM)

 
-- | Event stream of JOIN commands to join startup channels.
evtSendJoinInitialChans :: Event () -- ^ Event should fire when IRC connection is complete.
                        -> Behavior (I.IxSet ChatBuffer)
                        -> Reactive (Event IRCLine)
evtSendJoinInitialChans eConnInit bBuffers = do
    -- An event which sends IRC commands to join initial channels.
    bNetStBuffers <- fmap I.fromList <$> switchList (map bufStateBehavior
                                                     <$> I.toList <$> bBuffers)
    let eCmdJoinInitChans = joinLine <$> view ccBufferName <$> split eJoinInitChans
        eJoinInitChans = snapshot (const getActiveChans) eConnInit bNetStBuffers
        getActiveChans = filter isActiveChannel . I.toList
        isActiveChannel (ChatCoreChannelBuffer _ True) = True
        isActiveChannel _ = False
    return eCmdJoinInitChans

-- | Sends `PONG` in response to `PING`.
evtSendPong :: Event IRCLine -> Event IRCLine
evtSendPong eRecvLine = pongLine
                        <$> view ilBody
                        <$> filterCmd eRecvLine ICmdPing

--------------------------------------------------------------------------------
-- Acid State
--------------------------------------------------------------------------------

-- | Sets up listeners and other mechanisms to write changes to the acid state
-- database.
setupNetAcid :: AcidState ChatCoreState -> ChatUserName -> ChatCoreNetwork
             -> Behavior (I.IxSet ChatBuffer)
             -> Reactive (IO ()) -- ^ IO action to clean up.
setupNetAcid acid uName network bBuffers = do
    -- Create a behavior which turns all of the buffers into 
    bNetStBuffers <- fmap I.fromList <$> switchList (map bufStateBehavior
                                                     <$> I.toList <$> bBuffers)
    -- Link the bNetStBuffers behavior to the database.
    execAcidUpdates acid (SetNetworkBuffers uName netName
                          <$> updates bNetStBuffers)
  where
    netName = network ^. netStName


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- | Set up behaviors and events for the given network.
chatNetwork :: ChatUserName -> ChatCoreNetwork
            -> AcidState ChatCoreState
            -> Event ClientCommand
            -> Reactive ChatNetwork
chatNetwork uName network acid eClientCmd = do
  let netName = network ^. netStName
  rec
    (eInit', pushInit) <- newEvent
    let eInit = once eInit'

    (bConnStatus, eRecvLine, cleanIrcConn) <- behIrcConn eSendLine behIrcServer

    -- Some convenient events for handling connection and disconnection.
    let eConnected = void (filterE (==Connected) $ value bConnStatus)
        -- eDisconnected = void (filterE (==Disconnected) $ value bConnStatus)

    -- An event which fires when we have finished initializing the IRC connection.
    let eConnInit = void $ filterCmd eRecvLine (ICmdOther "001")

    -- Nick
    bUserNick <- behNick eRecvLine

    -- Buffers
    (bBuffers, cleanBufs) <- behBuffers uName network eInit bUserNick eRecvLine
    let bBufferList = I.toList <$> bBuffers
    let eBufferEvent = evtBufferEvent netName bBuffers


    -- Core Events
    let eCoreEvent = eBufferEvent

    -- Sending Lines
    eSendLine <- evtSendLine network
                             eConnected eConnInit
                             eRecvLine eClientCmd
                             bBuffers

    -- Acid
    cleanAcid <- setupNetAcid acid uName network bBuffers

  --------------------------------------------------------------------------------
  -- Cleanup
  --------------------------------------------------------------------------------

  cleanSendLnPrnt <- listen eSendLine (putStrLn . ("Sent line: "++) . show)
  cleanRecvLnPrnt <- listen eRecvLine (putStrLn . ("Received line: "++) . show)
  cleanBufEvtPrnt <- listen eBufferEvent (putStrLn . ("Buffer event: "++) . show)

  cleanConStatPrnt <- listen (value bConnStatus) print

  let bCleanupBuffers = map cleanupChatBuffer <$> bBufferList
  let clean = do
        cleanIrcConn
        cleanBufs
        cleanAcid
        join (sequence_ <$> sync (sample bCleanupBuffers))
        cleanConStatPrnt
        cleanRecvLnPrnt
        cleanSendLnPrnt
        cleanBufEvtPrnt

  pushInit ()

  return $ ChatNetwork
         netName bConnStatus bUserNick bBuffers eCoreEvent clean


getNetworkInfo :: ChatNetwork -> Behavior ChatNetworkInfo
getNetworkInfo net = ChatNetworkInfo
                     <$> pure (net ^. chatNetworkName)
                     <*> (net ^. bNetworkStatus)
                     <*> (net ^. bNetworkUserNick)


--------------------------------------------------------------------------------
-- Indexable
--------------------------------------------------------------------------------

instance I.Indexable ChatNetwork where
    empty = I.ixSet [ I.ixFun ((:[]) . view chatNetworkName) ]

-- Eq and Ord so we can use IxSet.
instance Eq ChatNetwork where
    a == b = netName a == netName b
      where
        netName = view chatNetworkName

instance Ord ChatNetwork where
    compare a b = netName a `compare` netName b
      where
        netName = view chatNetworkName
