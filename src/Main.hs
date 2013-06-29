-- | Main entry point to the program.

module Main where

import Server.Import
import Server.Server

-- | Main entry point.
main :: IO ()
main =
  withSocketsDo startAccepter
