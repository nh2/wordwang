{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server where

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (Exception)
import qualified Control.Exception as CE
import           Control.Monad (forever, forM, forM_, void, unless, filterM, when)
import           Data.Data (Data, Typeable)
import           Data.Foldable (foldlM)
import           System.FilePath ((</>))

import           Control.Concurrent.STM
import           Control.Monad.Trans (MonadIO(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           System.Directory
                 (createDirectory, doesDirectoryExist, getDirectoryContents,
                  doesFileExist)
import           Text.Printf (printf)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Digest.Pure.SHA (sha1, showDigest)
import           Network.WebSockets (Request, WebSockets, Hybi00, Sink)
import qualified Network.WebSockets as WS
import           System.Random (randomRIO)

import           Objects

data ServerState = ServerState
    { serverGroups    :: Map GroupId GroupChan
    , serverCounter   :: Int
    , closedStories :: [Story]
    }

type GroupChan = TChan GroupCmd
data GroupState = GroupState
    { groupSinks   :: Map UserId (Sink WebSocketProtocol)
    , groupCounter :: Int       -- ^ used to generate ids unique to the group
    }

type WebSocketProtocol = Hybi00

data GroupCmd
    = ClientCmdFwd UserId (Maybe (Sink WebSocketProtocol)) ClientCmd
    | Timeout

insertSink :: User -> Sink WebSocketProtocol -> GroupState -> GroupState
insertSink User{userId = uid} sink gs@GroupState{groupSinks = sinks} =
    gs{groupSinks = Map.insert uid sink sinks}

incCount :: GroupState -> (GroupState, Int)
incCount gs@GroupState{groupCounter = i} = (gs{groupCounter = i + 1}, i)

-- dummyGroup :: Group
-- dummyGroup = Group { groupId    = 2
--                    , groupUsers = users
--                    , groupStory = story
--                    , groupCloud = cloud
--                    }
--   where
--     francesco = User 7 "francesco"
--     andras = User 8 "andras"
--     alex = User 12 "alex"
--     users = Map.fromList $ map (\u -> (userId u, u)) [francesco, andras, alex]
--     block1 = Block 1 "I am potato"
--     block2 = Block 2 ", you are dragon"
--     block3 = Block 3 ", obama SUCKS!"
--     block4 = Block 4 ", n-word n-word n-word"
--     story = [block1, block2]
--     cloud = Cloud (Map.fromList [ (3, CloudItem block3 (Set.fromList [7]))
--                                 , (4, CloudItem block4 (Set.fromList [8, 12]))
--                                 ])

receiveClientCmd :: WebSockets WebSocketProtocol (Maybe ClientCmd)
receiveClientCmd =
    do msg <- WS.receive
       case msg of
           WS.ControlMessage _ ->
               return Nothing
           WS.DataMessage (WS.Text t) ->
               Just <$> maybe (fail "Failed to parse ClientCmd") return (Aeson.decode' t)
           WS.DataMessage (WS.Binary b) ->
               Just <$> maybe (fail "Failed to parse ClientCmd") return (Aeson.decode' b)

preJoin :: TVar ServerState -> Request -> WebSockets WebSocketProtocol ()
preJoin serverStateVar rq =
    do WS.acceptRequest rq
       mcmd <- receiveClientCmd
       case mcmd of
           Just cmd@(Join (JoinPayload _ mGroupId)) -> do
               liftIO $ print cmd
               uid <- makeId serverStateVar
               gchan <- getCreateGroup serverStateVar mGroupId
               sink <- WS.getSink
               liftIO $ atomically $
                        writeTChan gchan (ClientCmdFwd uid (Just sink) cmd)
               runUser gchan uid
           _ -> fail "Expecting join message"

runUser :: GroupChan -> UserId -> WebSockets WebSocketProtocol ()
runUser gchan uid = forever $ do
    _ <- liftIO $ printf "Waiting for msg for user %d\n" uid
    mcmd <- receiveClientCmd
    case mcmd of
        Just (Join _) ->
            fail "Expecting non-join cmd"
        Just cmd -> do _ <- liftIO $ printf "User %d got message\n" uid
                       liftIO $ atomically $ writeTChan gchan (ClientCmdFwd uid Nothing cmd)
        Nothing ->
            liftIO $ putStrLn "User got a control message"

makeId :: (MonadIO m) => TVar ServerState -> m Int
makeId serverStateVar = liftIO $ atomically $
    do serverState@ServerState {serverCounter = i} <- readTVar serverStateVar
       writeTVar serverStateVar (serverState {serverCounter = i + 1})
       return i

getCreateGroup :: (MonadIO m) => TVar ServerState -> Maybe GroupId -> m GroupChan
getCreateGroup serverStateVar Nothing = liftIO $
    do ServerState {serverGroups = gs} <- atomically $ readTVar serverStateVar
       nc <- randomRIO (0, _NEW_GROUP_P)
       -- Create a new group if there are no existing ones, or if d10 comes out 0.
       if Map.null gs || nc == 0
           then do putStrLn "Creating a new group"
                   gid <- makeId serverStateVar
                   createGroup serverStateVar gid
           else do _ <- printf "Looking up an existing group (out of %d groups)\n" (Map.size gs)
                   let gchans = map snd (Map.toList gs)
                   i <- randomRIO (0, Map.size gs - 1)
                   return (gchans !! i)
getCreateGroup serverStateVar (Just gid) = liftIO $
    do mgchan <- atomically $
                 do ServerState {serverGroups = gs} <- readTVar serverStateVar
                    return (Map.lookup gid gs)
       case mgchan of
           Just gchan -> return gchan
           Nothing    -> fail (printf "no such group %s" (show gid))

-- | seconds
_TICK_DELAY :: Int
_TICK_DELAY = 5

-- | Where are stories saved on disk?
_STORY_DIR :: FilePath
_STORY_DIR = "stories"

-- | There's a 1 in _NEW_GROUP_P chance of creating a new group when a user joins without
-- specifying one.
_NEW_GROUP_P :: Int
_NEW_GROUP_P = 10

createGroup :: (MonadIO m) => TVar ServerState -> GroupId -> m GroupChan
createGroup serverStateVar gid = liftIO $
    do let group = Group { groupId = gid
                         , groupUsers = Map.empty
                         , groupStory = []
                         , groupCloud = newCloud
                         }
       groupChan <- newTChanIO
       atomically $
           do serverState@ServerState {serverGroups = gs} <- readTVar serverStateVar
              writeTVar serverStateVar
                        (serverState { serverGroups = Map.insert gid groupChan gs })
       _ <- forkIO (runGroup serverStateVar
                             group
                             (GroupState {groupSinks = Map.empty, groupCounter = 0})
                             groupChan
                   `CE.finally` do _ <- printf "\n######## GROUP %d DIED ########\n\n" gid
                                   -- You should NEVER see the above message.
                                   return ())
       return groupChan

spawnFlushCloud :: (MonadIO m, Functor m) => Int -> GroupChan -> m ()
spawnFlushCloud secs gchan = void . liftIO . forkIO $
    do threadDelay (secs * 1000000); atomically $ writeTChan gchan Timeout

runGroup :: TVar ServerState -> Group -> GroupState -> GroupChan -> IO ()
runGroup serverStateVar
         group@Group{groupCloud = cloud@(Cloud votes _), groupStory = story} gs gchan =
    do gcmd <- atomically $ readTChan gchan
       case gcmd of
           ClientCmdFwd uid mSink cmd ->
               case (mSink, cmd) of
                   (Just sink, Join (JoinPayload uname _)) ->
                       do let user   = User uid uname
                              group' = insertUser user group
                              gs'    = insertSink user sink gs
                          WS.sendSink sink (WS.DataMessage
                                            (WS.Text (Aeson.encode (Refresh group' LoggedIn))))
                              `CE.catch` (\(_ :: CE.SomeException) -> return ())
                          broadcastRefresh group' NewJoin gs' >>= uncurry rec
                   (Nothing, Send blockContent) ->
                       do let (gs', bid) = incCount gs
                              block = Block bid blockContent
                              cloud' = insertBlock block uid cloud
                              group' = group {groupCloud = cloud'}
                          when (cloudEmpty cloud) (spawnFlushCloud _TICK_DELAY gchan)
                          broadcastRefresh group' CloudUpdate gs' >>= uncurry rec
                   (Nothing, Upvote bid) ->
                       case upvoteBlock bid uid cloud of
                           Just cloud' ->
                               do let group' = group {groupCloud = cloud'}
                                  broadcastRefresh group' CloudUpdate gs >>= uncurry rec
                           Nothing ->
                               do putStrLn "Upvote for nonexisting message, ignoring"
                                  rec group gs
                   _ -> do _ <- printf "got an unexpected command: %s\n" (show (uid, cmd))
                           runGroup serverStateVar group gs gchan
                           rec group gs
           Timeout ->
               case maxBlock (map snd (Map.toList votes)) of
                   Nothing -> fail "Timeout received with no blocks!"
                   Just Block{content = CloseBlock} ->
                       closeStory serverStateVar group gs gchan story
                   Just b -> do let group' = group { groupStory = story ++ [b]
                                                   , groupCloud = newCloud }
                                broadcastRefresh group' StoryUpdate gs >>= uncurry rec
  where
    rec group' gs' = runGroup serverStateVar group' gs' gchan
    maxBlock [] = Nothing
    maxBlock xs = Just (cloudBlock (maximum xs))

closeStory :: TVar ServerState -> Group -> GroupState -> GroupChan -> Story -> IO ()
closeStory ssvar group gs gchan story =
    do putStrLn "Closed story"
       atomically $ do sstate@ServerState{ closedStories = fss
                                         , serverGroups = grps } <- readTVar ssvar
                       writeTVar ssvar sstate{ closedStories = story : fss
                                             , serverGroups = Map.delete (groupId group) grps }
       -- This group is now closed, so drop all messages sent to it.
       forever $ do liftIO $ putStrLn "Dropping message"
                    _ <- atomically $ readTChan gchan
                    broadcastRefresh group NoChanges gs

broadcastCmd :: ServerCmd -> Group -> GroupState -> IO (Group, GroupState)
broadcastCmd cmd group gs@GroupState{groupSinks = sinks} =
    do _ <- printf "sending %s\n\n" (show cmd)
       foldlM sendSink' (group, gs) (Map.toList sinks)
  where
    sendSink' ( grp0@Group{groupUsers = users}
              , gs0@GroupState{groupSinks = sinks'})
              (uid, sink) =
        do CE.handle (\(_ :: CE.SomeException) ->
                       do _ <- putStrLn "There's a dead sink.  Removing it."
                          return ( grp0{groupUsers = Map.delete uid users}
                                 , gs0{groupSinks = Map.delete uid sinks'} )) $
               do putStrLn "Sending message to some sink"
                  WS.sendSink sink (WS.DataMessage (WS.Text (Aeson.encode cmd)))
                  return (grp0, gs0)

broadcastRefresh :: Group -> ServerCmdReason -> GroupState -> IO (Group, GroupState)
broadcastRefresh g reason = broadcastCmd (Refresh g reason) g

-- | Save all finished stories to "_STORY_DIR/<sha1 of story text>" as Show'd values.
saveStories :: (MonadIO m) => [Story] -> m ()
saveStories ss = liftIO $
    do _ <- printf "Saving %d stories\n" (length ss)
       dirExists <- doesDirectoryExist _STORY_DIR
       unless dirExists $ createDirectory _STORY_DIR
       forM_ ss $ \story ->
           do let storyText = BL.pack (show story)
              BL.writeFile (_STORY_DIR </> (showDigest (sha1 storyText))) storyText

-- | Load stories from "_STORY_DIR/*".  If the directory does not exist, returns
--   an empty list.
loadStories :: (MonadIO m) => m [Story]
loadStories = liftIO $
    do putStrLn "Loading stories"
       dirExists <- doesDirectoryExist _STORY_DIR
       if dirExists
           then do fs <- getDirectoryContents _STORY_DIR
                   fs' <- filterM doesFileExist fs
                   forM fs' $ \f ->
                       do text <- BL.readFile f
                          return (read (BL.unpack text))
           else return []

data Shutdown = Shutdown
    deriving (Data, Show, Typeable)

instance Exception Shutdown

-- | Start the WordWang server on the given host and port, return immediately,
--   and return an action that shuts down the server.
serve :: String -> Int -> IO (IO ())
serve host port =
    do initialStories <- loadStories
       serverState <- newTVarIO ServerState{ serverGroups  = Map.empty
                                           , serverCounter = 0
                                           , closedStories = initialStories }
       tid <- forkIO $ CE.handle (\(_ :: Shutdown) -> return ())
                                 (WS.runServer host port (preJoin serverState))
       return $ do CE.throwTo tid Shutdown
                   ServerState{closedStories = stories} <-
                       atomically (readTVar serverState)
                   saveStories stories
