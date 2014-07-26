-- | Main entry point to the program.

module Main where

import GHC.Server
import Network.Socket

-- | Main entry point.
main :: IO ()
main =
  withSocketsDo startAccepter
