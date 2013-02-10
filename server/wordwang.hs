-- | Command line tool to start the wordwang server.
module Main ( main ) where

import Control.Concurrent.MVar ( newEmptyMVar, putMVar, takeMVar )
import Server
import System.Posix.Signals ( Handler(..), installHandler, sigINT )
import qualified Control.Exception as CE

main :: IO ()
main = do
    shutdown <- serve "0.0.0.0" 8888
    done <- newEmptyMVar
    _ <- installHandler sigINT (Catch $ putMVar done ()) Nothing
    takeMVar done `CE.finally` do
        _ <- putStrLn "Shutting down... "
        shutdown
        _ <- putStrLn "done"
        return ()
