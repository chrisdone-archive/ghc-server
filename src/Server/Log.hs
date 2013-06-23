module Server.Log where

import Control.Monad.Trans
import Server.Types

logger :: MonadIO m => Log -> m ()
logger l = liftIO $
  case l of
    Notice n -> putStrLn ("Notice: " ++ n)
    Error n -> putStrLn ("ERROR: " ++ n)
    Debug n -> putStrLn ("Debug: " ++ n)
    _ -> return ()
