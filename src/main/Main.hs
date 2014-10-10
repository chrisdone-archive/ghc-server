-- | GHC service

module Main where

import GHC.Server
import GHC.Server.Logging

-- | Main entry point.
main :: IO ()
main = runLogging (startServer 1990)
