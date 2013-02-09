import Server

import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as CE
import           System.Posix.Signals ( Handler(..), installHandler, sigINT )

main :: IO ()
main =
    do shutdown <- serve "0.0.0.0" 8888
       done <- newEmptyMVar
       _ <- installHandler sigINT (Catch $ putMVar done ()) Nothing
       takeMVar done `CE.finally` (
           do _ <- putStrLn "Shutting down... "
              shutdown
              _ <- putStrLn "done"
              return ())
