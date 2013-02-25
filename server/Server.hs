{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}

module Server where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM ( atomically
                              , TVar, newTVarIO, readTVar, writeTVar
                              , TChan, newTChanIO, readTChan, writeTChan )
import Control.Exception ( Exception )
import Control.Monad ( forever, forM, forM_, void, unless, filterM, when )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Data ( Data, Typeable )
import Data.Digest.Pure.SHA ( sha1, showDigest )
import Data.Foldable ( foldlM )
import Data.Map ( Map )
import Network.WebSockets ( Request, WebSockets, Hybi00, Sink )
import Objects ( ServerCmd(..), Group(..), Block(..), ClientCmd(..), Cloud(..)
               , ServerCmdReason(..), BlockContent(..), Story, User(..), UserId
               , GroupId, JoinPayload(..), CloudItem(..)
               , newCloud, insertUser, insertBlock, cloudEmpty, upvoteBlock, cloudBlock )
import System.Directory ( createDirectory, doesDirectoryExist, getDirectoryContents, doesFileExist )
import System.FilePath ( (</>) )
import System.Log.Logger ( debugM )
import System.Random ( randomRIO )
import Text.Printf ( printf )
import qualified Control.Exception as CE
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import qualified Network.WebSockets as WS

------------------------------------------
-- Constants
------------------------------------------

-- | Logger name.
tag :: String
tag = "Server"

-- | Time for a round in seconds.
roundTime :: Int
roundTime = 7

-- | Where are stories saved on disk?
storyDir :: FilePath
storyDir = "stories"

-- | There's a 1 in newGroupP chance of creating a new group when a user joins without
-- specifying one.
newGroupP :: Int
newGroupP = 10

------------------------------------------
-- Types
------------------------------------------

-- | The global state shared among all threads in the server.
data ServerState = ServerState
    { serverGroups  :: Map GroupId (TChan GroupCmd)
    , serverCounter :: Int
    , closedStories :: [Story]
    }

-- | The state of each group thread.
data GroupState = GroupState
    { groupSinks   :: Map UserId (Sink WebSocketProtocol)
    , groupCounter :: Int       -- ^ used to generate ids unique to the group
    }

-- | We use the latest and greatest iteration of the WebSockets protocol.
type WebSocketProtocol = Hybi00

-- | The group threads may receive forwarded client commands, or timeouts, signaling the
-- end of a voting round.
data GroupCmd = ClientCmdFwd UserId (Maybe (Sink WebSocketProtocol)) ClientCmd
              | Timeout

-- | This signals the server thread to shutdown.
data Shutdown = Shutdown
    deriving (Data, Show, Typeable)

instance Exception Shutdown

------------------------------------------
-- User handler threads
------------------------------------------

-- to it.
waitForUserJoin :: TVar ServerState -> Request -> WebSockets WebSocketProtocol ()
waitForUserJoin serverStateVar rq = do
    liftIO $ debugM tag "waitForUserJoin"
    WS.acceptRequest rq
    mcmd <- receiveClientCmd
    case mcmd of
        Just cmd@(Join (JoinPayload _ mGroupId)) -> do
            liftIO $ print cmd
            uid <- makeId serverStateVar
            gchan <- getCreateGroup serverStateVar mGroupId
            sink <- WS.getSink
            liftIO $ atomically $ writeTChan gchan (ClientCmdFwd uid (Just sink) cmd)
            forwardUserCmds gchan uid
        _ ->
            fail "Expecting join message"

-- | Forward user commands to the connected group.
forwardUserCmds :: TChan GroupCmd -> UserId -> WebSockets WebSocketProtocol ()
forwardUserCmds gchan uid = forever $ do
    liftIO $ debugM tag (printf "forwardUserCmds: uid = %d\n" uid)
    mcmd <- receiveClientCmd
    case mcmd of
        Just (Join _) ->
            fail "Expecting non-join cmd"
        Just cmd -> do
            _ <- liftIO $ printf "User %d got message\n" uid
            liftIO $ atomically $ writeTChan gchan (ClientCmdFwd uid Nothing cmd)
        Nothing ->
            liftIO $ putStrLn "User got a control message"

-- | Receive a single 'ClientCmd'.  May fail due to parse errors.
receiveClientCmd :: WebSockets WebSocketProtocol (Maybe ClientCmd)
receiveClientCmd = do
    liftIO $ debugM tag "receiveClientCmd"
    msg <- WS.receive
    case msg of
        WS.ControlMessage _ ->
            return Nothing
        WS.DataMessage (WS.Text t) ->
            Just <$> maybe (fail "Failed to parse ClientCmd") return (Aeson.decode' t)
        WS.DataMessage (WS.Binary b) ->
            Just <$> maybe (fail "Failed to parse ClientCmd") return (Aeson.decode' b)

-- | Wait for a user to join the server, assign them to a group, and forward all commands
-- | Get the existing group for the given ID, or create a new one with it.
getCreateGroup :: (MonadIO m) => TVar ServerState -> Maybe GroupId -> m (TChan GroupCmd)
getCreateGroup serverStateVar Nothing = liftIO $ do
    liftIO $ debugM tag "getCreateGroup: no gid"
    ServerState {serverGroups = gs} <- atomically $ readTVar serverStateVar
    nc <- randomRIO (0, newGroupP)
    -- Create a new group if there are no existing ones, or if d10 comes out 0.
    if Map.null gs || nc == 0
        then do
            putStrLn "Creating a new group"
            gid <- makeId serverStateVar
            createGroup serverStateVar gid
        else do
            _ <- printf "Looking up an existing group (out of %d groups)\n" (Map.size gs)
            let gchans = map snd (Map.toList gs)
            i <- randomRIO (0, Map.size gs - 1)
            return (gchans !! i)
getCreateGroup serverStateVar (Just gid) = liftIO $ do
    liftIO $ debugM tag (printf "getCreateGroup: gid = %d\n" gid)
    mgchan <- atomically $ do
              ServerState {serverGroups = gs} <- readTVar serverStateVar
              return (Map.lookup gid gs)
    case mgchan of
        Just gchan -> return gchan
        Nothing    -> createGroup serverStateVar gid

-- | Create a group with the given 'GroupId'.  This ID /must/ be fresh.
createGroup :: (MonadIO m) => TVar ServerState -> GroupId -> m (TChan GroupCmd)
createGroup serverStateVar gid = liftIO $ do
    liftIO $ debugM tag (printf "createGroup: gid = %d\n" gid)
    let group = Group { groupId = gid
                      , groupUsers = Map.empty
                      , groupStory = []
                      , groupCloud = newCloud
                      }
    groupChan <- newTChanIO
    atomically $ do
        serverState@ServerState {serverGroups = gs} <- readTVar serverStateVar
        writeTVar serverStateVar
            (serverState { serverGroups = Map.insert gid groupChan gs
                         , serverCounter = max (gid + 1) (serverCounter serverState) })
    let gs = GroupState {groupSinks = Map.empty, groupCounter = 0}
    _ <- forkIO (runGroup serverStateVar group gs groupChan
                 `CE.finally` do
                     -- You should NEVER see this message.
                     _ <- printf "\n######## GROUP %d DIED ########\n\n" gid
                     return ())
    return groupChan

-- | Generate a new unique ID in the server's scope.  This is used to generate 'UserId's
-- and 'GroupId's.
makeId :: (MonadIO m) => TVar ServerState -> m Int
makeId serverStateVar = liftIO $ atomically $ do
    serverState@ServerState {serverCounter = i} <- readTVar serverStateVar
    writeTVar serverStateVar (serverState {serverCounter = i + 1})
    return i

------------------------------------------
-- Group threads
------------------------------------------

-- | Run a group thread.
runGroup :: TVar ServerState -> Group -> GroupState -> TChan GroupCmd -> IO ()
runGroup serverStateVar
         group@Group{ groupId    = gid
                    , groupCloud = cloud@(Cloud votes _)
                    , groupStory = story }
         gs
         gchan = do
    debugM tag (printf "runGroup: gid = %d\n" gid)
    gcmd <- atomically $ readTChan gchan
    case gcmd of
        ClientCmdFwd uid mSink cmd ->
            case (mSink, cmd) of
                (Just sink, Join (JoinPayload uname _)) -> do
                    let user   = User uid uname
                        group' = insertUser user group
                        gs'    = insertSink user sink gs
                    WS.sendSink sink (WS.DataMessage
                                      (WS.Text (Aeson.encode (Refresh group' LoggedIn))))
                        `CE.catch` (\(_ :: CE.SomeException) -> return ())
                    broadcastRefresh group' NewJoin gs' >>= uncurry rec
                (Nothing, Send blockContent) -> do
                    let (gs', bid) = incCount gs
                        block = Block bid blockContent
                        cloud' = insertBlock block uid cloud
                        group' = group {groupCloud = cloud'}
                    when (cloudEmpty cloud) (spawnFlushCloud roundTime)
                    broadcastRefresh group' CloudUpdate gs' >>= uncurry rec
                (Nothing, Upvote bid) ->
                    case upvoteBlock bid uid cloud of
                        Just cloud' -> do
                            let group' = group {groupCloud = cloud'}
                            broadcastRefresh group' CloudUpdate gs >>= uncurry rec
                        Nothing -> do
                            putStrLn "Upvote for nonexisting message, ignoring"
                            rec group gs
                _ -> do
                     _ <- printf "got an unexpected command: %s\n" (show (uid, cmd))
                     runGroup serverStateVar group gs gchan
                     rec group gs
        Timeout ->
            case maxBlock (map snd (Map.toList votes)) of
                Nothing ->
                    fail "Timeout received with no blocks!"
                Just Block{content = CloseBlock} ->
                    closeStory serverStateVar group gs gchan story
                Just b -> do
                    let group' = group { groupStory = story ++ [b]
                                       , groupCloud = newCloud }
                    broadcastRefresh group' StoryUpdate gs >>= uncurry rec
  where
    rec group' gs' = runGroup serverStateVar group' gs' gchan

    maxBlock :: [CloudItem] -> Maybe Block
    maxBlock [] = Nothing
    maxBlock xs = Just (cloudBlock (maximum xs))

    insertSink :: User -> Sink WebSocketProtocol -> GroupState -> GroupState
    insertSink User{ userId = uid } sink gs0@GroupState{ groupSinks = sinks } =
        gs0{ groupSinks = Map.insert uid sink sinks }

    incCount :: GroupState -> (GroupState, Int)
    incCount gs0@GroupState{ groupCounter = i } =
        (gs0{ groupCounter = i + 1 }, i)

    spawnFlushCloud :: (MonadIO m, Functor m) => Int -> m ()
    spawnFlushCloud secs = void . liftIO . forkIO $ do
        threadDelay (secs * 1000000)
        atomically $ writeTChan gchan Timeout

-- | Close the given story.  The group will cease to function as expected beyond this
-- point.
closeStory :: TVar ServerState -> Group -> GroupState -> TChan GroupCmd -> Story -> IO ()
closeStory ssvar group gs gchan story = do
    debugM tag (printf "closeStory: gid = %d\n" (groupId group))
    putStrLn "Closed story"
    atomically $ do
        sstate@ServerState{ closedStories = fss
                          , serverGroups = grps } <- readTVar ssvar
        writeTVar ssvar sstate{ closedStories = story : fss
                              , serverGroups = Map.delete (groupId group) grps }
    -- This group is now closed, so drop all messages sent to it.
    forever $ do
        liftIO $ putStrLn "Dropping message"
        _ <- atomically $ readTChan gchan
        broadcastRefresh group NoChanges gs

-- | Broadcast a single command to all connected users.  Prune the ones for which sending
-- failed.
broadcastCmd :: ServerCmd -> Group -> GroupState -> IO (Group, GroupState)
broadcastCmd cmd group gs@GroupState{groupSinks = sinks} = do
    debugM tag (printf "broadcastCmd: gid = %d" (groupId group))
    debugM tag (printf "cmd = %s" (show cmd))
    foldlM sendSink' (group, gs) (Map.toList sinks)
  where
    sendSink' ( grp0@Group{groupUsers = users}
              , gs0@GroupState{groupSinks = sinks'})
              (uid, sink) = do
        CE.handle (\(_ :: CE.SomeException) -> do
                        _ <- putStrLn "There's a dead sink.  Removing it."
                        return ( grp0{groupUsers = Map.delete uid users}
                               , gs0{groupSinks = Map.delete uid sinks'} )) $ do
            putStrLn "Sending message to some sink"
            WS.sendSink sink (WS.DataMessage (WS.Text (Aeson.encode cmd)))
            return (grp0, gs0)

-- | Broadcast a 'Refresh' command with 'broadcastCmd'.
broadcastRefresh :: Group -> ServerCmdReason -> GroupState -> IO (Group, GroupState)
broadcastRefresh g reason = broadcastCmd (Refresh g reason) g

------------------------------------------
-- Stories' persistence
------------------------------------------

-- | Save all finished stories to "storyDir/<sha1 of story text>" as Show'd values.
saveStories :: (MonadIO m) => [Story] -> m ()
saveStories ss = liftIO $ do
    debugM tag (printf "saveStories: # = %d" (length ss))
    dirExists <- doesDirectoryExist storyDir
    unless dirExists $ createDirectory storyDir
    forM_ ss $ \story -> do
        let storyText = BL.pack (show story)
        BL.writeFile (storyDir </> (showDigest (sha1 storyText))) storyText

-- | Load stories from "storyDir/*".  If the directory does not exist, returns an empty
--   list.
loadStories :: (MonadIO m) => m [Story]
loadStories = liftIO $ do
    debugM tag "loadStories"
    dirExists <- doesDirectoryExist storyDir
    if dirExists
        then do
            fs <- getDirectoryContents storyDir
            fs' <- filterM doesFileExist fs
            forM fs' $ \f -> do
                text <- BL.readFile f
                return (read (BL.unpack text))
        else
            return []

------------------------------------------
-- Main server function
------------------------------------------

-- | Start the WordWang server on the given host and port, return immediately,
--   and return an action that shuts down the server.
serve :: String -> Int -> IO (IO ())
serve host port = do
    debugM tag "serve"
    initialStories <- loadStories
    serverState <- newTVarIO ServerState{ serverGroups  = Map.empty
                                        , serverCounter = 0
                                        , closedStories = initialStories }
    tid <- forkIO $ CE.handle (\(_ :: Shutdown) -> return ())
                              (WS.runServer host port (waitForUserJoin serverState))
    return $ do
        CE.throwTo tid Shutdown
        ServerState{closedStories = stories} <- atomically (readTVar serverState)
        saveStories stories
