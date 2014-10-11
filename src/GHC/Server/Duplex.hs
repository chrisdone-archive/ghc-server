{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- | The Duplex monad, used for full duplex communication between
-- server and client commands.

module GHC.Server.Duplex where

import GHC.Compat
import GHC.Server.Types

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Map (Map)

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

-- | Transform over Ghc, running the transformed GHC in isolation.
withGhc :: (Inputish i,Outputish o)
        => DuplexT Ghc i o r -> Duplex i o r
withGhc m =
  do st <- ask
     ghcChan <- asks duplexRunGhc
     io (do result <- newEmptyMVar
            writeChan ghcChan
                      (do v <- runReaderT (runDuplexT m)
                                          st
                          io (putMVar result v))
            takeMVar result)


-- | Run a Ghc action in another thread. Transform over Ghc, running
-- the transformed GHC in isolation.
forkGhc :: (MonadDuplex i o m)
        => Ghc () -> m ThreadId
forkGhc m =
  do st <- ask
     ghcChan <- asks duplexRunGhc
     io (forkIO (do result <- newEmptyMVar
                    writeChan ghcChan
                              (do v <- m
                                  io (putMVar result v))
                    takeMVar result))

-- | Run a Ghc action in another thread. Transform over Ghc, running
-- the transformed GHC in isolation.
forkWithGhc :: (MonadDuplex i o m)
            => DuplexT Ghc i o () -> m ThreadId
forkWithGhc m =
  do st <- ask
     ghcChan <- asks duplexRunGhc
     io (forkIO (do result <- newEmptyMVar
                    writeChan ghcChan
                              (do v <- runReaderT (runDuplexT m)
                                                  st
                                  io (putMVar result v))
                    takeMVar result))

-- | Get the global module infos value.
getModuleInfos :: (MonadDuplex i o m) => m (Map ModuleName ModInfo)
getModuleInfos =
  do var <- getModuleInfosVar
     infos <- liftIO (atomically (readTVar var))
     return infos

-- | Get the global module infos value.
getModuleInfosVar :: (MonadDuplex i o m) => m (TVar (Map ModuleName ModInfo))
getModuleInfosVar =
  asks (stateModuleInfos . duplexState)
