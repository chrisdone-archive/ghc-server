module GHC.Server.Log where

import Control.Monad
import Control.Monad.Trans
import GHC.Server.Types

logger :: MonadIO m => Log -> m ()
logger l =
  liftIO (when False
               (case l of
                  Notice n -> putStrLn ("Notice: " ++ n)
                  Error n -> putStrLn ("ERROR: " ++ n)
                  Debug n -> putStrLn ("Debug: " ++ n)
                  Fatal n -> putStrLn ("FATAL: " ++ n)))
