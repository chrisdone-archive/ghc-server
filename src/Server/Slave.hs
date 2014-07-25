{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}

-- | GHC slave.

module Server.Slave where

import Server.Import
import System.Environment

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
     userFlags <- makeUserFlags
     (dflags',_,_)   <- parseDynamicFlags initialDynFlags (map (mkGeneralLocated "flag") userFlags)
     _ <- setSessionDynFlags (dflags' { hscTarget = HscInterpreted
                                      , ghcLink = LinkInMemory })
     (dflags'',_packageids) <- liftIO (initPackages dflags')
     io (putStrLn (showppr dflags'' _packageids))
     _ <- setSessionDynFlags dflags''
     mapM parseImportDecl imports >>= setContext
     return ()

  where imports = ["import Prelude"]

-- | Make user flags, if HSENV is activated then use the
-- PACKAGE_DB_FOR_GHC environment variable for package flags.
makeUserFlags :: Ghc [String]
makeUserFlags =
  do env <- liftIO getEnvironment
     case lookup "HSENV" env >> lookup "PACKAGE_DB_FOR_GHC" env of
       Just flags -> return (words flags)
       Nothing -> return []

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

-- | Pretty print something to string.
showppr :: Outputable a => DynFlags -> a -> String
showppr dflags = showSDocForUser dflags neverQualify . ppr
