{-# LANGUAGE FlexibleContexts #-}
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
recv :: (MonadDuplex i o m)
     => m i
recv =
  do inp <-  (asks duplexIn)
     io (readChan inp)

-- | Send an output.
send :: (MonadDuplex i o m)
     => o -> m ()
send o =
  do out <-  (asks duplexOut)
     io (writeChan out o)

-- -- | Run the GHC action in isolation.
-- liftGhc :: Ghc r -> Duplex i o r
-- liftGhc m =
--   do ghcChan <- asks duplexRunGhc
--      io (do result <- newEmptyMVar
--             io (writeChan ghcChan
--                           (do v <- m
--                               io (putMVar result v)))
--             takeMVar result)

-- | Transform over Ghc, running the transformed GHC in isolation.
withGhc :: (Inputish i,Outputish o)
        => DuplexT Ghc i o r -> Duplex i o r
withGhc m =
  do st <- DuplexT ask
     ghcChan <- DuplexT (asks duplexRunGhc)
     io (do result <- newEmptyMVar
            io (writeChan ghcChan
                          (do v <- runReaderT (runDuplexT m) st
                              io (putMVar result v)))
            takeMVar result)
