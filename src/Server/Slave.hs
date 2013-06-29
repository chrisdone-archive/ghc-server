-- | GHC slave.

module Server.Slave where

import Server.Import

import DynFlags
import ErrUtils
import GHC
import GHC.Paths
import Packages
import Outputable

-- | Start a new GHC slave.
newSlave :: ThreadId -> IO Slave
newSlave main =
 do inChan <- newChan
    tid <- forkIO (gcatch (defaultErrorHandler defaultFatalMessager
                                               defaultFlushOut
                                               (runGhc (Just libdir)
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
     setSessionDynFlags initialDynFlags
     (dflags',_,_)   <- parseDynamicFlags initialDynFlags (map (mkGeneralLocated "flag") flags)
     _pkgs           <- setSessionDynFlags dflags' { ghcLink = LinkInMemory
                                                   , hscTarget = HscInterpreted
                                                   , ghcMode = CompManager
                                                   , log_action = logAction
                                                   }

     dflags          <- getSessionDynFlags
     (dflags,_pkgs) <- liftIO $ initPackages dflags
     setSessionDynFlags dflags
     mapM (fmap IIDecl . parseImportDecl) imports >>= setContext
     return ()

  where flags = ["-package ghc","-isrc"] :: [String]
        imports = ["import Prelude"]

-- | Run a GHC slave. This will receive commands and execute them
-- sequentially in a single thread.
runSlave slaveIn =
  do actions <- liftIO (getChanContents slaveIn)
     forM_ actions protect

  where protect (onError,m) =
          gcatch m
                 (\se@(SomeException e) ->
                   do logger (Error ("Slave: " ++ show e))
                      liftIO (onError se))

logAction :: DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
logAction = defaultLogAction
