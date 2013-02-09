{-# LANGUAGE GeneralizedNewtypeDeriving, DefaultSignatures, ScopedTypeVariables #-}
module Log where

import System.Console.ANSI
import System.IO
import Control.Monad.Reader
import Data.Time.Clock
import Data.List
import Control.Applicative
import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent

import Prelude hiding (log)

data Severity
    = SUCCESS | INFO | WARNING | ERROR
      deriving (Show)

-- SGR attributes

class (Monad m) => MonadLog m where
    log :: Severity -> String -> m ()
    default log :: (MonadTrans t, MonadLog m) => Severity -> String -> t m ()
    log sev s = lift (log sev s)

instance (MonadLog m) => MonadLog (StateT s m) where
instance (MonadLog m) => MonadLog (ReaderT s m) where

logS :: MonadLog m => String -> m ()
logS = log SUCCESS

logI :: MonadLog m => String -> m ()
logI = log INFO

logW :: MonadLog m => String -> m ()
logW = log WARNING

logE :: MonadLog m => String -> m ()
logE = log ERROR

sgr :: Severity -> [SGR]
sgr SUCCESS = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green]
sgr INFO = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
sgr WARNING = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Yellow]
sgr ERROR = [SetConsoleIntensity BoldIntensity, SetBlinkSpeed SlowBlink, SetColor Foreground Vivid Red]


withSgr :: [SGR] -> String -> String
withSgr sg s = setSGRCode sg ++ s ++ setSGRCode []

newtype LogT m a
    = LogT { unLogT :: ReaderT (String, TChan (Severity, String)) m a }
      deriving (Functor, Monad, MonadIO, MonadTrans)

type Log = LogT IO

logger :: String -> TChan (Severity, String) -> IO ()
logger filename chan = do 
  fh <- liftIO $ openFile filename AppendMode
  forever $ do
    (sev, str) <- atomically $ readTChan chan
    currTime <- floor . toRational . utctDayTime <$> liftIO getCurrentTime
    let line f = timeStr currTime ++ " | " ++
                 (f $ padS 7 (show sev)) ++ " | " ++ str ++ "\n"
        terminal = line (withSgr (sgr sev))
        fileLine = line id
    hPutStr stderr terminal
    hPutStr fh fileLine
    hFlush fh

forkLog :: String -> LogT IO () -> LogT IO ThreadId
forkLog tname l = do
  (_, chan) <- LogT ask
  liftIO . forkIO $ runReaderT (unLogT l) (tname, chan)

runLogT :: (MonadIO m) => String -> String -> LogT m a -> m a
runLogT tname filename l = do
  chan <- liftIO newTChanIO
  _ <- liftIO . forkIO $ logger filename chan
  runReaderT (unLogT l) (tname, chan)

pad0 :: Int -> String -> String
pad0 n = reverse . take n . (++ repeat '0') . reverse

padS :: Int -> String -> String
padS n = take n . (++ repeat ' ')

timeStr :: Int -> String
timeStr i =
    let sec = i `mod` 60
        mn  = i `div` 60 `mod` 60
        hr  = i `div` 3600
        f   = pad0 2 . show
    in intercalate ":" . map f $ [hr, mn, sec]


instance (MonadIO m, Functor m) => MonadLog (LogT m) where
    log sev s = do
      (tname, chan) <- LogT ask
      liftIO . atomically . writeTChan chan $ (sev, "[" ++ tname ++ "] " ++ s)

