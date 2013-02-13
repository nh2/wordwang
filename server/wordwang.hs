-- | Command line tool to start the wordwang server.
module Main ( main ) where

import Control.Concurrent.MVar ( newEmptyMVar, putMVar, takeMVar )
import Server ( serve )
import System.FilePath ( (</>) )
import System.IO ( stdout )
import System.Log.Handler.Simple ( verboseStreamHandler, fileHandler )
import System.Log.Logger ( infoM, Priority(..)
                         , updateGlobalLogger, setHandlers, setLevel, rootLoggerName )
import System.Posix.Signals ( Handler(..), installHandler, sigINT )
import Text.Printf ( printf )
import qualified Control.Exception as CE

------------------------------------------
-- Constants
------------------------------------------

-- | Logger name.
tag :: String
tag = "wordwang"

-- | Hostname for server.
host :: String
host = "0.0.0.0"

-- | Port number for server.
port :: Int
port = 8888

------------------------------------------
-- Functions
------------------------------------------

main :: IO ()
main = do
    setupLogging
    infoM tag "starting wordwang"
    shutdown <- serve host port
    infoM tag (printf "serving on %s:%d" host port)
    done <- newEmptyMVar
    _ <- installHandler sigINT (Catch $ putMVar done ()) Nothing
    takeMVar done `CE.finally` do
        infoM tag "shutting down"
        shutdown
        infoM tag "shutdown done"

setupLogging :: IO ()
setupLogging = do
    consoleH <- verboseStreamHandler stdout DEBUG
    fileH <- fileHandler ("log" </> "server.log") DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    updateGlobalLogger rootLoggerName $ \logger ->
        flip setHandlers logger [consoleH, fileH]
