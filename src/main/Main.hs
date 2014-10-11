-- | Main entry point to the program.

module Main where

import GHC.Server
import GHC.Server.Logging

import Data.Maybe
import System.Environment

-- | Main entry point.
main :: IO ()
main = do args <- fmap parse getArgs
          runLogging (startServer (fromMaybe 5233 (lookup "port" args)))
  where parse = go
          where go ("--port":port:xs) = ("port",read port) : go xs
                go (_:xs) = go xs
                go [] = []
