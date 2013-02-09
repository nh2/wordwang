{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}
module Server where

import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as CE
import           Data.Data (Data, Typeable)
import           Control.Exception ( Exception )
import           Control.Monad (forever, forM_, void)
import           Control.Monad.Trans (MonadIO(..))
import           Data.List (maximumBy)
import           Data.Ord (comparing)

import           Control.Concurrent.STM
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Text.Printf (printf)

import qualified Data.Aeson as Aeson
import           Network.WebSockets
                 (Request, WebSockets, runServer, Hybi00, Sink, getSink)
import qualified Network.WebSockets as WS

import           Objects

data ServerState = ServerState
    { serverGroups  :: Map GroupId GroupChan
    , serverCounter :: Int
    }

type GroupChan = TChan GroupCmd
data GroupState = GroupState
    { groupSinks :: Map UserId (Sink WebSocketProtocol)
    , groupCount :: Int
    }

type WebSocketProtocol = Hybi00

data GroupCmd
    = ClientCmdFwd UserId (Maybe (Sink WebSocketProtocol)) ClientCmd
    | Timeout

dummyGroup :: Group
dummyGroup = Group { groupId    = 2
                   , groupUsers = users
                   , groupStory = story
                   , groupCloud = cloud
                   }
  where
    francesco = User 7 "francesco"
    andras = User 8 "andras"
    alex = User 12 "alex"
    users = Map.fromList $ map (\u -> (userId u, u)) [francesco, andras, alex]
    block1 = Block 1 "I am potato"
    block2 = Block 2 ", you are dragon"
    block3 = Block 3 ", obama SUCKS!"
    block4 = Block 4 ", n-word n-word n-word"
    story = [block1, block2]
    cloud = Cloud (Map.fromList [ (3, (block3, Set.fromList [7]))
                                , (4, (block4, Set.fromList [8, 12]))
                                ])

receiveClientCmd :: WebSockets WebSocketProtocol ClientCmd
receiveClientCmd =
    do msg <- WS.receiveData
       maybe (fail "Failed to parse ClientCmd") return (Aeson.decode' msg)

preJoin :: TVar ServerState -> Request -> WebSockets WebSocketProtocol ()
preJoin serverStateVar rq =
    do WS.acceptRequest rq
       cmd <- receiveClientCmd
       case cmd of
           Join (JoinPayload _ mGroupId) -> do
               liftIO $ print cmd
               uid <- makeId serverStateVar
               gchan <- getCreateGroup serverStateVar mGroupId
               sink <- getSink
               liftIO $ atomically $
                        writeTChan gchan (ClientCmdFwd uid (Just sink) cmd)
               runUser gchan uid
           _ -> fail "Expecting join message"

runUser :: GroupChan -> UserId -> WebSockets WebSocketProtocol ()
runUser gchan uid = forever $ do
    _ <- liftIO $ printf "Waiting for msg for user %s\n" (show uid)
    cmd <- receiveClientCmd
    case cmd of
        Join _ -> fail "Expecting non-join cmd"
        _ -> liftIO $ atomically $ writeTChan gchan (ClientCmdFwd uid Nothing cmd)

makeId :: (MonadIO m) => TVar ServerState -> m Int
makeId serverStateVar = liftIO $ atomically $
    do serverState@ServerState {serverCounter = i} <- readTVar serverStateVar
       writeTVar serverStateVar (serverState {serverCounter = i + 1})
       return i

getCreateGroup :: (MonadIO m) => TVar ServerState -> Maybe GroupId -> m GroupChan
getCreateGroup serverStateVar Nothing = liftIO $
    do gid <- makeId serverStateVar; createGroup serverStateVar gid
getCreateGroup serverStateVar (Just gid) = liftIO $
    do mgchan <- atomically $
                 do ServerState {serverGroups = gs} <- readTVar serverStateVar
                    return (Map.lookup gid gs)
       case mgchan of
           Just gchan -> return gchan
           Nothing    -> fail (printf "no such group %s" (show gid))

-- seconds
_TICK_DELAY :: Int
_TICK_DELAY = 5

createGroup :: (MonadIO m) => TVar ServerState -> GroupId -> m GroupChan
createGroup serverStateVar gid = liftIO $
    do let group = Group { groupId = gid
                         , groupUsers = Map.empty
                         , groupStory = []
                         , groupCloud = Cloud Map.empty
                         }
       groupChan <- newTChanIO
       atomically $
           do serverState@ServerState {serverGroups = gs} <- readTVar serverStateVar
              writeTVar serverStateVar
                        (serverState { serverGroups = Map.insert gid groupChan gs })
       spawnFlushCloud _TICK_DELAY groupChan
       _ <- forkIO (runGroup group
                             (GroupState {groupSinks = Map.empty, groupCount = 0})
                             groupChan)
       return groupChan

spawnFlushCloud :: (MonadIO m, Functor m) => Int -> GroupChan -> m ()
spawnFlushCloud secs gchan = void . liftIO . forkIO $
    do threadDelay (secs * 1000000); atomically $ writeTChan gchan Timeout

runGroup :: Group -> GroupState -> GroupChan -> IO ()
runGroup group@Group{ groupUsers = gusers
                    , groupCloud = Cloud gcloud
                    , groupStory = story }
         gs@GroupState{ groupSinks = sinks
                      , groupCount = count }
         gchan =
    do gcmd <- atomically $ readTChan gchan
       case gcmd of
           ClientCmdFwd uid mSink cmd ->
               case (mSink, cmd) of
                   (Just sink, Join (JoinPayload uname _)) ->
                       do let gs' = gs {groupSinks = Map.insert uid sink sinks}
                              group' = group { groupUsers = Map.insert uid
                                                            (User uid uname) gusers }
                          broadcastCmd (Refresh group') gs'
                          runGroup group' gs' gchan
                   (Nothing, Send blockContent) ->
                       do let bid = count
                              gs' = gs {groupCount = count + 1}
                              gcloud' = Map.insert bid
                                        (Block bid blockContent, Set.fromList [uid])
                                        gcloud
                              group' = group {groupCloud = Cloud gcloud'}
                          broadcastCmd (Refresh group') gs'
                          runGroup group' gs' gchan
                   (Nothing, Upvote bid) ->
                       do let mgvotes = Map.lookup bid gcloud
                          case mgvotes of
                              Nothing -> undefined
                              Just (b, uids) ->
                                  do let gcloud' = Map.insert bid
                                                   (b, Set.insert uid uids) gcloud
                                         group' = group {groupCloud = Cloud gcloud'}
                                     broadcastCmd (Refresh group') gs
                                     runGroup group' gs gchan
                   _ -> do liftIO (print (uid, cmd)); runGroup group gs gchan
           Timeout ->
               case maxBlock (map snd (Map.toList gcloud)) of
                   Nothing -> do spawnFlushCloud _TICK_DELAY gchan
                                 runGroup group gs gchan
                   Just b -> do let group' = group { groupStory = b : story
                                                   , groupCloud = Cloud Map.empty }
                                spawnFlushCloud _TICK_DELAY gchan
                                broadcastCmd (Refresh group') gs
                                runGroup group' gs gchan
    where
      maxBlock [] = Nothing
      maxBlock xs = Just (fst (maximumBy (comparing (Set.size . snd)) xs))

broadcastCmd :: ServerCmd -> GroupState -> IO ()
broadcastCmd cmd GroupState{groupSinks = sinks} =
    do putStrLn (show cmd)
       forM_ (map snd (Map.toList sinks)) sendSink'
  where
    sendSink' sink = WS.sendSink sink (WS.DataMessage (WS.Text (Aeson.encode cmd)))

data Shutdown = Shutdown
              deriving ( Data, Show, Typeable )

instance Exception Shutdown

-- | Start the WordWang server on the given host and port, return immediately, and return
-- an action that shuts down the server.
serve :: String -> Int -> IO (IO ())
serve host port =
    do tid <- forkIO $
           CE.handle (\(_ :: Shutdown) -> return ()) $ do
               serverState <- newTVarIO (ServerState Map.empty 0)
               runServer host port (preJoin serverState)
       return (CE.throwTo tid Shutdown)
