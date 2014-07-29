-- | Imported running actions for doing IO evaluation in the server.

module GHC.Server.IO (runIO) where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as C
import           System.IO
import           System.Posix.IO

-- | Run the given IO action, capturing its stdout output and passing
-- it to the continuation.
runIO :: IO b                  -- ^ Action to run.
      -> (ByteString -> IO ()) -- ^ Continuation to accept stdout.
      -> IO b
runIO m cont =
  do -- Shadow stdout.
     origStdout <- dup stdOutput
     (r,w) <- createPipe
     _ <- dupTo w stdOutput
     closeFd w
     h <- fdToHandle r
     hSetBuffering stdout NoBuffering
     runConcurrently
        $ Concurrently (sourceHandle h $$ C.mapM_ cont)
       *> Concurrently (m `finally` hFlush stdout
                          `finally` dupTo origStdout stdOutput)
