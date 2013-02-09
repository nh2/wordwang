{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Monad (forever)

import           Control.Monad.Trans (MonadIO(..))
-- import qualified Data.Text.IO as Text

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Network.WebSockets (Request, WebSockets, runServer, Hybi00, Sink, getSink)
import qualified Network.WebSockets as WS
import qualified Data.Aeson as Aeson

import           Objects

import           Control.Concurrent ( forkIO )
import           Control.Concurrent.STM

import           Text.Printf ( printf )

data ServerState = ServerState { serverGroups  :: Map GroupId GroupChan
                               , serverCounter :: Int }

type GroupChan = TChan GroupCmd

type WebSocketProtocol = Hybi00

data GroupCmd = ClientMsgFwd UserId (Maybe (Sink WebSocketProtocol)) ClientCmd

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

preJoin :: TVar ServerState -> Request -> WebSockets WebSocketProtocol ()
preJoin serverStateVar rq = WS.acceptRequest rq >> go
  where
    go = forever $
       do msg <- WS.receiveData
          case Aeson.decode' msg :: Maybe ClientCmd of
              Just cmd@(Join (JoinPayload _ mGroupId)) -> do
                  liftIO $ print cmd
                  uid <- makeId serverStateVar
                  gchan <- getCreateGroup serverStateVar mGroupId
                  sink <- getSink
                  liftIO $ atomically $ writeTChan gchan (ClientMsgFwd uid (Just sink) cmd)
                  runUser serverStateVar uid
              _ -> do
                  liftIO $ putStrLn "Failed parse of join"
                  return ()

runUser :: TVar ServerState -> UserId -> WebSockets WebSocketProtocol ()
runUser _serverStateVar uid = do
    _ <- liftIO $ printf "Running user %s\n" (show uid)
    return ()
          -- liftIO (Text.putStrLn msg)
          -- WS.sendTextData (Aeson.encode (Aeson.toJSON (Refresh dummyGroup)))

makeId :: (MonadIO m) => TVar ServerState -> m Int
makeId serverStateVar = liftIO $ atomically $ do
    serverState@ServerState {serverCounter = i} <- readTVar serverStateVar
    writeTVar serverStateVar (serverState { serverCounter = i + 1 })
    return i

getCreateGroup :: (MonadIO m) => TVar ServerState -> Maybe GroupId -> m GroupChan
getCreateGroup serverStateVar Nothing = liftIO $ do
    gid <- makeId serverStateVar
    createGroup serverStateVar gid
getCreateGroup serverStateVar (Just gid) = liftIO $ do
    mgchan <- atomically $ do
        ServerState {serverGroups = gs} <- readTVar serverStateVar
        return (Map.lookup gid gs)
    case mgchan of
        Just gchan ->
            return gchan
        Nothing ->
            fail (printf "no such group %s" (show gid))

createGroup :: (MonadIO m) => TVar ServerState -> GroupId -> m GroupChan
createGroup serverStateVar gid = liftIO $ do
    let groupState = Group { groupId = gid
                           , groupUsers = Map.empty
                           , groupStory = []
                           , groupCloud = Cloud Map.empty
                           }
    groupChan <- newTChanIO
    atomically $ do
        serverState@ServerState {serverGroups = gs} <- readTVar serverStateVar
        writeTVar serverStateVar (serverState { serverGroups = Map.insert gid groupChan gs })
    _ <- forkIO (runGroup groupState groupChan)
    return groupChan

runGroup :: Group -> GroupChan -> IO ()
runGroup _ _ = return ()

serve :: String -> Int -> IO ()
serve host port = do
    serverState <- newTVarIO (ServerState Map.empty 1)
    runServer host port (preJoin serverState)
