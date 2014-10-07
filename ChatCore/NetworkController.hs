{-# LANGUAGE UndecidableInstances #-}
-- | The network controller is an actor responsible for managing a user's
-- connection to a single IRC network.
-- It handles client commands and generates core events for that network.
module ChatCore.NetworkController
    ( NetCtlHandle
    , NetCtlActorMsg (..)
    , startNetCtl
    ) where

import Control.Applicative
import Control.Concurrent.Actor
import Control.Concurrent.Async
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Acid
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.IxSet as I
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Time
import Network
import Safe
import System.FilePath

import ChatCore.ChatLog
import ChatCore.Events
import ChatCore.IRC
import ChatCore.State
import ChatCore.Types
import ChatCore.Util
import {-# SOURCE #-} ChatCore.UserController

-- {{{ State and types

data BufUserType = BufUserOp | BufUserVoice | BufUserNormal

-- | State structure for a user in a buffer.
data BufferUser = BufferUser
    { _buNick :: T.Text
    , __buPerms :: BufUserType
    }
makeLenses ''BufferUser

-- | State structure for a buffer (a channel or private message query).
data NetBufState = NetBufState
    { _bufUsers :: [BufferUser]
    -- True while we're receiving a NAMES list for this buffer. If an
    -- RPL_NAMREPLY message is received while this is False, it will be set to
    -- True, and the names list will be cleared for reloading.
    -- When the RPL_ENDOFNAMES message is received, this will be set to False.
    , _bufReceivingNames :: Bool
    }
makeLenses ''NetBufState
blankNetBufState :: NetBufState
blankNetBufState = NetBufState [] False

-- | Data structure which stores the state of a chat session on a network.
data NetCtlState = NetCtlState
    { _nsId             :: ChatNetworkName  -- The ID name of this network.
    , _netUserName      :: UserName         -- Username of the user who owns this network.
    , _netNick          :: Nick             -- The user's current nick.
    , _netNicks         :: [Nick]           -- Other nicks that can be used.
    , _netBuffers       :: M.Map BufferName NetBufState -- Map of buffers the user is in.
    , _netHost          :: HostName
    , _netPort          :: PortID
    , _netIRCConn       :: IRCConnection
    , _userCtlAddr      :: UserCtlHandle    -- The user controller.
    , _netChatLog       :: ChatLog          -- The chat log for this network.
    , _netRecvAsync     :: Async ()         -- Async thread for receiving messages.
    }
makeLenses ''NetCtlState

-- | Monad constraint type for the network controller actor.
type NetCtlActor m =
    ( MonadActor NetCtlActorMsg m
    , MonadState NetCtlState m
    , MonadResource m, MonadIRC m
    , MonadLogger m
    , MonadReader (AcidState ChatCoreState) m)

instance (NetCtlActor m) => MonadIRC m where
    ircConn = use netIRCConn


instance HasChatCoreUser NetCtlState where
    getCurrentUserName = view netUserName

instance HasChatCoreNetwork NetCtlState where
    getCurrentNetworkName = view nsId

-- }}}

-- {{{ External Interface

-- NOTE: If you change this, be sure to update the hs-boot file too.
data NetCtlActorMsg
    = NetCtlClientCmd   ClientCommand
    | NetCtlIRCLine     IRCLine

instance ActorMessage NetCtlActorMsg

type NetCtlHandle = ActorHandle NetCtlActorMsg

-- | Starts a network controller for the given network.
-- Throws an exception if the network doesn't exist.
startNetCtl :: (Functor m, MonadIO m, HasAcidState ChatCoreState m) =>
    UserName -> ChatNetworkName -> UserCtlHandle -> m NetCtlHandle
startNetCtl uName netName ucAddr = do
    acid <- getAcidState
    -- Find the network with the given name.
    net <- fromJust <$> (queryM $ GetUserNetwork netName uName)
    -- Open the chat log for this network.
    chatLog <- liftIO $ mkChatLog ("log" </> (T.unpack (net ^. networkName)))
    -- Start the network controller.
    hand <- liftIO $
        spawnActor $
        (flip runReaderT) acid $
        initNetCtlActor $ NetCtlState
            { _nsId = net ^. networkName
            , _netUserName = uName
            , _netNicks = net ^. networkNicks
            , _netNick = undefined
            , _netBuffers = M.empty
            -- Will connect on startup.
            , _netHost = undefined
            , _netPort = undefined
            , _netIRCConn = undefined
            , _netRecvAsync = undefined
            , _netChatLog = chatLog
            , _userCtlAddr = ucAddr
            }
    return hand

-- }}}

-- {{{ Main functions

-- | Initializes a network controller with the given state.
initNetCtlActor :: NetCtlState -> ReaderT (AcidState ChatCoreState) (ActorM NetCtlActorMsg) ()
initNetCtlActor = evalStateT $ runResourceT $ runStderrLoggingT $ do
    -- Look up a list of servers to try.
    servers <- getServerList
    let server = headNote ("Network has no servers.") servers
        host = T.unpack (server ^. serverHost)
        port = PortNumber $ fromIntegral (server ^. serverPort)
    netHost .= host
    netPort .= port
    $(logInfoS) "NetCtl" ("Connecting to IRC: "
                          <> tshow host <> " " <> tshow port)
    (_, conn) <- allocate
        (connectIRC host port)
        disconnectIRC
    netIRCConn .= conn
    $(logInfoS) "NetCtl" "Connected to IRC server. Registering."

    -- Start the receiver thread.
    me <- self
    recvSrc <- sourceRecvLine
    (_, rth) <- allocate
        (async (recvSrc $$ CL.mapM_ (send me . NetCtlIRCLine)))
        cancel
    netRecvAsync .= rth

    -- Initialize the IRC connection.
    nick <- head <$> use netNicks
    sendNickCmd nick
    sendUserCmd nick "Chat Core"
    -- Start the main function.
    networkController

-- | The actual network controller actor.
networkController :: (NetCtlActor m) => m ()
networkController = do
    -- Read a message.
    msg <- receive
    case msg of
         NetCtlClientCmd ccmd -> handleClientCmd ccmd
         NetCtlIRCLine line -> do
             $(logDebugS) "NetCtl" ("Got IRC line: " <> tshow line)
             handleIRCEvent line
    networkController

-- }}}

-- {{{ Utility functions

-- {{{ Database Functions

getServerList :: (NetCtlActor m) => m [ChatCoreNetServer]
getServerList = viewNetwork networkServers

-- | Gets a list of channels to join after connecting to IRC.
getInitialChans :: (NetCtlActor m) => m [T.Text]
getInitialChans =
        map (view ccBufferName)
    <$> filter isActiveChannel
    <$> I.toList
    <$> viewNetwork networkBuffers
  where
    isActiveChannel (ChatCoreChannelBuffer _ True) = True
    isActiveChannel _ = False

-- }}}

-- {{{ Sending and Logging Events

-- | Sends the given core event to the user controller.
ncSendCoreEvent :: (NetCtlActor m) => CoreEvent -> m ()
ncSendCoreEvent evt = do
    uc <- use userCtlAddr
    ucSendCoreEvt uc evt

-- | Logs the given event.
logEvent :: (NetCtlActor m) => BufferName -> LogEvent -> m ()
logEvent buf evt = do
    time <- liftIO getCurrentTime
    chatLog <- use netChatLog
    let logLine = LogLine buf time evt
    liftIO $ writeLogLine chatLog logLine

-- | Generates a status message for the given numeric reply message arguments
-- and content.
logMsgForReply :: (NetCtlActor m) => [T.Text] -> Maybe T.Text -> m T.Text
logMsgForReply args' bodyM = do
    myNick <- use netNick
    -- Strip the current nick from the front of the args list if it is present.
    let args = if length args' > 0 && head args' == myNick
                  then tail args'
                  else args'
        argStr = T.intercalate ", " args
    -- The log message is the argument list separated by commas, followed
    -- by a space and then the message body (if present).
    return (argStr <> maybe "" (" " <>) bodyM)

-- }}}

-- {{{ Debugging Stuff

getUserList :: (NetCtlActor m) => BufferName -> m [Nick]
getUserList buf = do
    map (^. buNick) <$> use (netBuffer buf . bufUsers)

-- }}}

-- {{{ Utility Lenses and other functions for getting information.

-- | A lens pointing to the state variable for the buffer with the given name.
netBuffer :: BufferName -> Traversal' NetCtlState NetBufState
netBuffer name = netBuffers . ix name

-- | Maps the given function over all of the network buffers which satisfy the
-- given predicate.
forBufsWhere :: (NetCtlActor m) =>
    (NetBufState -> Bool) -> (BufferName -> NetBufState -> m NetBufState) -> m ()
forBufsWhere predicate action = do
    bufs <- use netBuffers
    newBufs <- iforM bufs $ \key buf ->
        if predicate buf
           then action key buf
           else return (buf)
    netBuffers .= newBufs


-- | True if the given buffer contains the given nick.
hasUser :: Nick -> NetBufState -> Bool
hasUser nick (NetBufState { _bufUsers = users }) =
    elemOf (each . buNick) nick users

-- }}}

-- }}}

-- {{{ Actions

-- | Checks which channels we should join on startup and joins them.
joinInitialChans :: (NetCtlActor m) => m ()
joinInitialChans = do
    chans <- getInitialChans
    forM_ chans sendJoinCmd

-- }}}

-- {{{ Handler functions

-- {{{ Command Handlers

-- | Handles a client event for the given network controller.
handleClientCmd :: (NetCtlActor m) => ClientCommand -> m ()

handleClientCmd (JoinChannel _ chan) = sendJoinCmd chan
handleClientCmd (PartChannel _ chan msg) =
    sendPartCmd chan $ fromMaybe "Leaving" msg

handleClientCmd (SendMessage _ dest msg MtPrivmsg) =
    sendPrivMsgCmd dest msg
handleClientCmd (SendMessage _ dest msg MtNotice) =
    sendNoticeCmd dest msg

-- }}}

-- {{{ Messages

handlePrivmsg :: (NetCtlActor m) => IRCUser -> BufferName -> T.Text -> m ()
handlePrivmsg user dest msg = do
    bufferEvent dest $ ReceivedMessage user msg MtPrivmsg

handleNotice :: (NetCtlActor m) => IRCUser -> BufferName -> T.Text -> m ()
handleNotice user dest msg = do
    bufferEvent dest $ ReceivedMessage user msg MtNotice

-- }}}

-- {{{ Current user join, part, and nick

handleSelfJoin :: (NetCtlActor m) => IRCUser -> BufferName -> m ()
handleSelfJoin user dest = do
    -- Add the buffer to our buffer list.
    -- TODO: Send an event indicating a buffer was added.
    netBuffers %= (M.insert dest $ blankNetBufState)
    bufferEvent dest $ UserJoin user

handleSelfPart :: (NetCtlActor m) => IRCUser -> BufferName -> Maybe T.Text -> m ()
handleSelfPart user dest msgM = do
    -- Remove the buffer from our list.
    netBuffers %= sans dest
    bufferEvent dest $ UserPart user msgM

handleSelfNickChange :: (NetCtlActor m) => IRCUser -> Nick -> m ()
handleSelfNickChange user newNick = do
    netNick .= newNick
    networkEvent $ UserNickChange user newNick

-- }}}

-- {{{ Other user join, part, nick, and quit

handleOtherJoin :: (NetCtlActor m) => IRCUser -> BufferName -> m ()
handleOtherJoin user dest = do
    -- Add the user to the buffer.
    netBuffer dest . bufUsers <>= [BufferUser (user ^. iuNick) BufUserNormal]
    users <- getUserList dest
    $(logDebugS) "NetCtl" ("User joined. Users: " <> T.unwords users)
    bufferEvent dest $ UserJoin user

handleOtherPart :: (NetCtlActor m) => IRCUser -> BufferName -> Maybe T.Text -> m ()
handleOtherPart user dest msgM = do
    -- Remove the user from the buffer.
    netBuffer dest . bufUsers %= filter (\u -> u ^. buNick /= (user ^. iuNick))
    users <- getUserList dest
    $(logDebugS) "NetCtl" ("User left. Users: " <> T.unwords users)
    bufferEvent dest $ UserPart user msgM

handleQuit :: (NetCtlActor m) => IRCUser -> Maybe T.Text -> m ()
handleQuit user@(IRCUser { _iuNick = nick }) msgM = do
    -- For all buffers containing the given nick.
    forBufsWhere (hasUser nick) $ \bufId buf -> do
        bufferEvent bufId $ UserQuit user msgM
        -- Remove the user from the buffer.
        return (buf & bufUsers %~ filter (\u -> u ^. buNick /= nick))

handleOtherNickChange :: (NetCtlActor m) => IRCUser -> Nick -> m ()
handleOtherNickChange user@(IRCUser { _iuNick = nick }) newNick = do
    -- For all buffers containing the given nick.
    forBufsWhere (hasUser nick) $ \bufId buf -> do
        bufferEvent bufId $ OtherNickChange user newNick
        -- Update the user's nick in the user list.
        return (buf & bufUsers %~
            map (\u -> if u ^. buNick == nick
                          then (buNick .~ newNick) u
                          else u))

-- }}}

-- {{{ Misc Events (RPL_WELCOME, NAMES, etc)

-- RPL_WELCOME
handleWelcome :: (NetCtlActor m) => [T.Text] -> Maybe T.Text -> m ()
handleWelcome args msgM = do
    case msgM of
         Just msg -> statusMessage Nothing msg
         Nothing -> return ()
    -- Take the first argument as our nick.
    -- NOTE: I'm not sure if this is part of the standard, but both EsperNet
    -- and Freenode send the nick as the first argument of the welcome message
    -- and many other numeric reply messages.
    $(logDebugS) "NetCtl" ("Nick is " <> head args)
    netNick .= head args
    -- Join whatever channels we're supposed to join on startup.
    -- TODO: If we need to auth with NickServ, this should be done *after*
    -- we've done that. Quassel doesn't wait for NickServ auth before joining
    -- channels, and as a result, if you're in channels which require you to
    -- have a registered nick, Quassel will fail to join them on startup. This
    -- is annoying. Chat Core should not have this problem.
    joinInitialChans


-- {{{ NAMES list.

-- RPL_NAMREPLY
-- Since I couldn't find much information about this numeric reply, I'm making
-- some notes of it here.
-- The RPL_NAMREPLY (353) reply has 3 arguments and a body.
-- The first argument is the current user's nick.
-- The second argument is either "=", "*", or "@", for public, private, and
-- secret channels. Chat Core currently just ignores this argument.
-- The third argument is the channel name.
-- The body is a space separated list of users in the channel.
handleNamesList :: (NetCtlActor m) => BufferName -> [Nick] -> m ()
handleNamesList chan names = do
    $(logDebugS) "NetCtl" ("Names: " <> T.unwords names)
    -- Append the list of names to the buffer's name list.
    netBuffer chan %= execState bufAction
  where
    bufAction = do
        -- Check if we've been receiving names.
        receiving <- use bufReceivingNames
        -- If so, just continue adding users to the name list.
        -- Otherwise, clear the name list and start receiving names.
        if receiving then return () else bufUsers .= []
        bufReceivingNames .= True
        bufUsers <>= map bufUserFromStr names
    bufUserFromStr userStr =
        case hChar of
             '@' -> BufferUser (T.tail userStr) BufUserOp
             '+' -> BufferUser (T.tail userStr) BufUserVoice
             _ -> BufferUser userStr BufUserNormal
      where
        hChar = T.head userStr

-- RPL_ENDOFNAMES
-- When the name list ends, reset the bufRecvingNames variable to False and
-- send out a name list update event.
handleNamesListEnd :: (NetCtlActor m) => BufferName -> m ()
handleNamesListEnd chan = do
    $(logDebugS) "NetCtl" "End of name list."
    netBuffer chan . bufReceivingNames .= False

-- }}}

-- }}}

-- {{{ Other Numeric Replies

handleNumeric :: (NetCtlActor m) =>
    Maybe IRCSource -> (Char, Char, Char) -> [T.Text] -> Maybe T.Text -> m ()

handleNumeric senderM ('4', _, _) args msgM = do
    logMsg <- logMsgForReply args msgM
    statusMessage senderM ("Error: " <> logMsg)

handleNumeric senderM (_, _, _) args msgM = do
    logMsg <- logMsgForReply args msgM
    statusMessage senderM logMsg

-- }}}

-- {{{ Handle Buffer Events

-- | Sends a buffer event for the given buffer.
bufferEvent :: (NetCtlActor m) => BufferName -> BufferEvent -> m ()
bufferEvent bufId evt = do
    netId <- use nsId
    -- Send the event and log it.
    ncSendCoreEvent $ BufCoreEvent netId bufId evt
    logEvent bufId $ LogBufEvent evt


-- }}}

-- {{{ More network event stuff

-- | Logs the given network event and forwards it to the core controller.
networkEvent :: (NetCtlActor m) => NetworkEvent -> m ()
networkEvent evt = do
    netId <- use nsId
    -- Send the event and log it to the -net- buffer.
    ncSendCoreEvent $ NetCoreEvent netId evt


-- | Writes the given message to the network buffer for this IRC network.
statusMessage :: (NetCtlActor m) => Maybe IRCSource -> T.Text -> m ()
statusMessage senderM msg = do
    let sender = case senderM of
            Just (UserSource user) -> user ^. iuNick
            Just (ServerSource src) -> src
            _ -> "*"
    bufferEvent "-status-" $ StatusMessage sender msg

-- }}}


-- {{{ Handle IRC Events
-- Some nonsense to hide the IRC line handling crap and map it to the neater
-- handler functions defined above.

-- | Takes two `NetCtlActor` actions and executes the first action if the given
-- user is the current user. Otherwise, executes the second action.
switchIsMe :: (NetCtlActor m) => IRCUser -> m () -> m () -> m ()
switchIsMe user ifSo ifNot = do
    myNick <- use netNick
    if myNick == (user ^. iuNick) then ifSo else ifNot


handleIRCEvent :: (NetCtlActor m) => IRCLine -> m ()
handleIRCEvent (IRCLine _ (ICmdPing) _ senderM) = sendPongCmd senderM


handleIRCEvent (IRCLine (Just (UserSource user)) (ICmdPrivmsg) [dest] (Just msg)) =
    handlePrivmsg user dest msg
handleIRCEvent (IRCLine (Just (UserSource user)) (ICmdNotice) [dest] (Just msg)) =
    handleNotice user dest msg


-- JOIN
handleIRCEvent (IRCLine (Just (UserSource user)) (ICmdJoin) [dest] Nothing) =
    switchIsMe user
        (handleSelfJoin user dest)
        (handleOtherJoin user dest)
-- PART
handleIRCEvent (IRCLine (Just (UserSource user)) (ICmdPart) [dest] msgM) =
    switchIsMe user
        (handleSelfPart user dest msgM)
        (handleOtherPart user dest msgM)
-- QUIT
handleIRCEvent (IRCLine (Just (UserSource user)) (ICmdQuit) [] msgM) =
    switchIsMe user
        (return ()) -- We don't do anything here.
        (handleQuit user msgM)
-- NICK
handleIRCEvent (IRCLine (Just (UserSource user)) (ICmdNick) [] (Just newNick)) =
    switchIsMe user
        (handleSelfNickChange user newNick)
        (handleOtherNickChange user newNick)


-- RPL_WELCOME
handleIRCEvent (IRCLine _ (ICmdOther (['0', '0', '1'])) args msgM) =
    handleWelcome args msgM


-- RPL_NAMREPLY
handleIRCEvent (IRCLine _ (ICmdOther (['3', '5', '3'])) [_, _, chan] (Just names)) =
    handleNamesList chan $ T.words names

-- RPL_ENDOFNAMES
handleIRCEvent (IRCLine _ (ICmdOther (['3', '6', '6'])) [_, chan] _) =
    handleNamesListEnd chan


-- RPL_ENDOFNAMES
handleIRCEvent (IRCLine sourceM (ICmdOther ([c1, c2, c3])) args bodyM) =
    handleNumeric sourceM (c1, c2, c3) args bodyM


handleIRCEvent line = do
    $(logErrorS) "NetCtl" ("Unhandled IRC line: " <> tshow line)

-- }}}

-- }}}

