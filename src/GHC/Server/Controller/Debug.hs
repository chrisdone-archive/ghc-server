-- | Debugging controller.

module GHC.Server.Controller.Debug where

import GHC.Server.Types

-- | Ping/pong.
ping :: Integer -> Returns Integer
ping = return
