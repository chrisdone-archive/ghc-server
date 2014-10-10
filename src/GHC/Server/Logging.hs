-- |

module GHC.Server.Logging where

import Control.Monad.Logger
import Control.Monad.Reader

--------------------------------------------------------------------------------
-- Logging functions

-- | Run the logging monad.
runLogging :: MonadIO m => LoggingT m () -> m ()
runLogging = runStdoutLoggingT
