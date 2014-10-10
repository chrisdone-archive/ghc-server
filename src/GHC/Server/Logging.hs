-- |

module GHC.Server.Logging where

import Control.Monad.Logger
import Control.Monad.Reader

--------------------------------------------------------------------------------
-- Logging functions

runLogging :: MonadIO m => LoggingT m () -> m ()
runLogging = runStdoutLoggingT
