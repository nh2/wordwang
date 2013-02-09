import Server

import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as CE
import           System.Posix.Signals ( Handler(..), installHandler, sigINT )
import           Text.Printf (printf)

main :: IO ()
main =
    do shutdown <- serve "0.0.0.0" 8888
       done <- newEmptyMVar
       _ <- installHandler sigINT (Catch $ putMVar done ()) Nothing
       takeMVar done `CE.finally` (
           do _ <- printf "Shutting down... "
              shutdown
              _ <- printf "done\n"
              return ())
