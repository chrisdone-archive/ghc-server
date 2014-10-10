{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- | The Duplex monad, used for full duplex communication between
-- server and client commands.

module GHC.Server.Duplex where

import GHC.Compat
import GHC.Server.Types

import Control.Concurrent
import Control.Monad.Reader

--------------------------------------------------------------------------------
-- Duplexing monad

-- | Receive an input.
recv :: (MonadIO m,Inputish i) => DuplexT m i o i
recv =
  do inp <- DuplexT (asks duplexIn)
     io (readChan inp)

-- | Send an output.
send :: (MonadIO m,Outputish o) => o -> DuplexT m i o ()
send o =
  do out <- DuplexT (asks duplexOut)
     io (writeChan out o)

-- | Run the GHC action in isolation.
liftGhc :: Ghc r -> Duplex i o r
liftGhc m =
  do ghcChan <- DuplexT (asks duplexRunGhc)
     io (do result <- newEmptyMVar
            io (writeChan ghcChan
                          (do v <- m
                              io (putMVar result v)))
            takeMVar result)

-- | Transform over Ghc.
withGhc :: DuplexT Ghc i o r -> Duplex i o r
withGhc m =
  do st <- DuplexT ask
     ghcChan <- DuplexT (asks duplexRunGhc)
     io (do result <- newEmptyMVar
            io (writeChan ghcChan
                          (do v <- runReaderT (runDuplexT m) st
                              io (putMVar result v)))
            takeMVar result)
