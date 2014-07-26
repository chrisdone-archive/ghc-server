module GHC.Server.Log where

import Control.Concurrent.MVar
import Control.Monad.Trans
import GHC.Server.Types
import System.IO
import System.IO.Unsafe

printLock = unsafePerformIO (newMVar ())

logger :: MonadIO m => Log -> m ()
logger l =
  liftIO
    (withMVar
       printLock
       (const
          (case l of
             Notice n -> puts ("Notice: " ++ n)
             Error n -> puts ("ERROR: " ++ n)
             Debug n -> puts ("Debug: " ++ n)
             Fatal n -> puts ("FATAL: " ++ n))) )
  where puts = hPutStrLn stderr
