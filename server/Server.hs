module Server where

import           Control.Monad (forever)

import           Control.Monad.Trans (liftIO)
import           Data.Text (Text)
import qualified Data.Text.IO as Text

import           Network.WebSockets

echo :: TextProtocol p => Request -> WebSockets p ()
echo rq = do acceptRequest rq; go
  where
    go = forever $
       do msg <- receiveData
          liftIO (Text.putStrLn msg)
          sendTextData (msg :: Text)
