-- | Debugging commands.

module GHC.Server.Commands.Debug where

import GHC.Server.Types

-- | Ping/pong.
ping :: Integer -> Returns Integer
ping = return
