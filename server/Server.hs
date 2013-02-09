{-# LANGUAGE OverloadedStrings #-}
module Server (server) where

import           Control.Monad (forever)

import           Control.Monad.Trans (liftIO)
import qualified Data.Text.IO as Text

import           Network.WebSockets (Request, WebSockets, TextProtocol)
import qualified Network.WebSockets as WS
import qualified Data.Aeson as Aeson

import           Objects

import qualified Data.Map as Map
import qualified Data.Set as Set

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
    cloud = Cloud (Map.fromList [ (3, CloudItem block3 (Set.fromList [7]))
                                , (4, CloudItem block4 (Set.fromList [8, 12]))
                                ])

server :: TextProtocol p => Request -> WebSockets p ()
server rq = do WS.acceptRequest rq; go
  where
    go = forever $
       do msg <- WS.receiveData
          liftIO (Text.putStrLn msg)
          WS.sendTextData (Aeson.encode (Aeson.toJSON (Refresh dummyGroup)))
