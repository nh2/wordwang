{-# LANGUAGE ScopedTypeVariables #-}
module Server where

import           Control.Monad.Trans (liftIO)

import           Data.Text (Text)
import qualified Data.Text.IO as Text

import           Network.WebSockets

echo :: TextProtocol p => WebSockets p ()
echo =
    do dat :: Text <- receiveData
       liftIO (Text.putStrLn dat)
       sendTextData dat
