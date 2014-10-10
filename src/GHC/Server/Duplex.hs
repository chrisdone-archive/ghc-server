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
recv :: Inputish i => Duplex i o i
recv =
  do inp <- Duplex (asks stateIn)
     io (readChan inp)

-- | Send an output.
send :: Outputish o => o -> Duplex i o ()
send o =
  do out <- Duplex (asks stateOut)
     io (writeChan out o)

-- | Run the GHC action in isolation.
withGhc :: ((forall a. Duplex i o a -> IO a) -> Ghc r) -> Duplex i o r
withGhc m =
  do st <- Duplex ask
     ghcChan <- Duplex (asks stateGhc)
     io (do result <- newEmptyMVar
            io (writeChan ghcChan
                          (do v <- m (\mdup ->
                                        liftIO (runReaderT (runDuplex mdup)
                                                           st))
                              io (putMVar result v)))
            takeMVar result)
