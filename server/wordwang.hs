{-# LANGUAGE OverloadedStrings #-}
import Server

import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as CE
import           System.Environment (getArgs)

import           System.Posix.Signals (Handler(..), installHandler, sigINT)

defaultHtmlDir :: FilePath
defaultHtmlDir = "html"

main :: IO ()
main =
    do args <- getArgs
       let htmlDir = case args of
                         []     -> defaultHtmlDir
                         (fp:_) -> fp
       shutdown <- serve "0.0.0.0" 8888 htmlDir
       done <- newEmptyMVar
       _ <- installHandler sigINT (Catch $ putMVar done ()) Nothing
       takeMVar done `CE.finally` (
           do _ <- putStrLn "Shutting down... "
              shutdown
              _ <- putStrLn "done"
              return ())
