-- | Imported running actions for doing IO evaluation in the server.

module GHC.Server.IO (runIO) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           System.IO
import           System.Posix.IO

-- | Run the given IO action, capturing its stdout output and passing
-- it to the continuation in <=1KB chunks.
runIO :: IO b                  -- ^ Action to run.
      -> (ByteString -> IO ()) -- ^ Continuation to accept stdout.
      -> IO b
runIO m cont =
  do -- Shadow stdout.
     origStdout <- dup stdOutput
     (r,w) <- createPipe
     _ <- dupTo w stdOutput
     hSetBuffering stdout NoBuffering
     h <- fdToHandle r
     -- Start consumer.
     doneVar <- newMVar False
     finishedConsuming <- newEmptyMVar
     void
       (forkIO
          (do void
                (try
                   (fix
                      (\loop ->
                         do some <- S.hGetNonBlocking h 1024
                            if S.null some
                               then do done <- readMVar doneVar
                                       if done
                                          then hClose h
                                          else do threadDelay (1000 * 100)
                                                  loop
                               else do cont some
                                       loop))
                 :: IO (Either IOException ()))
              putMVar finishedConsuming ()))
     -- Start user action.
     v <- m
     -- End.
     -- Tell the consumer that writing has finished.
     swapMVar doneVar True
     -- Wait until consumer has finished consuming.
     takeMVar finishedConsuming
     -- Restore stdout.
     wh <- fdToHandle w
     hClose wh
     dupTo origStdout stdOutput
     return v
