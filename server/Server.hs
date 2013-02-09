module Server (server) where

import           Control.Monad (forever)

import           Control.Monad.Trans (liftIO)
import           Data.Text (Text)
import qualified Data.Text.IO as Text

import           Network.WebSockets (Request, WebSockets, TextProtocol)
import qualified Network.WebSockets as WS

server :: TextProtocol p => Request -> WebSockets p ()
server rq = do WS.acceptRequest rq; go
  where
    go = forever $
       do msg <- WS.receiveData
          liftIO (Text.putStrLn msg)
          WS.sendTextData (msg :: Text)
