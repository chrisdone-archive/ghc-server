{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall #-}

-- | GHC slave.

module Server.Slave where

import Server.Import

import GHC.Compat

-- | Start a new GHC slave.
newSlave :: ThreadId -> IO Slave
newSlave main =
 do inChan <- newChan
    tid <- forkIO (gcatch (defaultErrorHandler (runGhc (Just libdir)
                                                       (do logger (Notice "Starting GHC ...")
                                                           initializeSlave
                                                           runSlave inChan)))
                          (\(SomeException e) -> do
                            logger (Error ("GHC threw an exception:\n    "
                                          ++ show e
                                          ++ "\nThrowing up to main thread ..."))
                            throwTo main e))
    return (Slave inChan tid)

-- | Initialize the GHC service.
initializeSlave :: Ghc ()
initializeSlave =
  do initialDynFlags <- getSessionDynFlags
     _ <- setSessionDynFlags initialDynFlags
     (dflags',_,_)   <- parseDynamicFlags initialDynFlags (map (mkGeneralLocated "flag") flags')
     _pkgs           <- setSessionDynFlags dflags' { ghcLink    = LinkInMemory
                                                   , hscTarget  = HscInterpreted
                                                   , ghcMode    = CompManager
                                                   }

     dflags <- getSessionDynFlags
     (dflags'',_pkgs) <- io (initPackages dflags)
     _ <- setSessionDynFlags dflags''
     mapM parseImportDecl imports >>= setContext
     return ()

  where flags' = [] :: [String]
        imports = ["import Prelude"]

-- | Run a GHC slave. This will receive commands and execute them
-- sequentially in a single thread.
runSlave
  :: (Server.Import.MonadIO m, ExceptionMonad m,
      GHC.Compat.MonadIO m) =>
     Chan (SomeException -> IO b, m b) -> m ()
runSlave slaveInp =
  do actions <- liftIO (getChanContents slaveInp)
     forM_ actions protect
  where protect (onError,m) =
          gcatch m
                 (\se@(SomeException e) ->
                   do logger (Error ("Slave: " ++ show e))
                      liftIO (onError se))
