module GHC.Server.IO (runIO) where

import GHC.IO.Handle
{-import GHC.IO.Handle.FD-}
import System.IO
import System.Posix.IO

runIO :: IO b -> IO b
runIO m =
  do origStdout <- dup stdOutput
     (r,w) <- createPipe
     dupTo w stdOutput
     hSetBuffering stdout NoBuffering
     h <- fdToHandle r
     v <- m
     wh <- fdToHandle w
     hClose wh
     dupTo origStdout stdOutput
     putStrLn "Me!"
     hGetContents h >>= putStr
     return v
