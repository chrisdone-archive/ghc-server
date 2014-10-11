{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Ghc monad actions.

module GHC.Server.Model.Ghc (initializeGhc,withMessages,loadedImports) where

import           GHC.Compat
import           GHC.Server.Cabal
import           GHC.Server.Defaults
import           GHC.Server.Types

import           Control.Concurrent.STM
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.List
import           Data.Monoid
import qualified Data.Text as T
import           Linker
import           System.Environment

-- | Initialize the GHC service.
initializeGhc :: (MonadLogger m,GhcMonad m)
                   => m ()
initializeGhc =
  do (libincs,exts,pkgs) <- liftIO getDependencyInfo
     initialDynFlags <- getSessionDynFlags
     userFlags <- fmap (<> concat [exts,initFlags,deps pkgs,src libincs]) makeUserFlags
     (dflags',_,_) <- parseDynamicFlags
                        (initialDynFlags {hscTarget = HscAsm
                                         ,ghcLink = LinkInMemory
                                         ,ghcMode = CompManager
                                         ,optLevel = 0})
                        (map (mkGeneralLocated "flag") userFlags)
     let dflags'' = dflags'
     _ <- setSessionDynFlags dflags''
     (dflags''',packageids) <- liftIO (initPackages dflags'')
     _ <- setSessionDynFlags dflags'''
     mapM parseImportDecl necessaryImports >>=
       setContext
     liftIO (initDynLinker dflags''')
     $(logInfo)
       ("User flags: " <>
        T.pack (unwords userFlags))
     $(logInfo)
       ("Packages: " <>
        T.pack (unwords (map (showppr dflags''') packageids)))
  where initFlags =
          ["-fobject-code"
          ,"-dynamic-too"
          ,"-v1"
          ,"-optP-include"
          ,"-optPdist/build/autogen/cabal_macros.h"] <>
          ["-fdefer-type-errors" | ghcVersion >= Ghc78]
        deps [] = []
        deps xs =
          ["-hide-all-packages"] <>
          map (\pkg -> "-package " <> renderPackageId pkg) xs
        src [] = []
        src xs = map (\x -> "-i" <> x) xs

-- | Handle any messages coming from GHC. GHC seems to disregard
-- resetting the 'log_action' for some reason, so we set a log action
-- which will read from a var for its action and that var will be
-- reset once the action is done. Any further messages broadcast on
-- that handler will be printed in debugging mode as bogus.
withMessages :: (MonadDuplex i o m,GhcMonad m)
             => (DynFlags -> Severity -> SrcSpan -> SDoc -> Duplex i o ()) -> m a -> m a
withMessages handler m =
  do handlerV <- liftIO (atomically (newTVar handler))
     st <- ask
     oldFlags <- getSessionDynFlags
     setLogAction
       (\df sv sp _ msg ->
          do f <- atomically (readTVar handlerV)
             runReaderT (runDuplexT (f df (translateSeverity df sv msg) sp msg))
                        st)
     v <- m
     newFlags <- getSessionDynFlags
     void (setSessionDynFlags newFlags {log_action = log_action oldFlags})
     liftIO (atomically
               (writeTVar handlerV
                          (\df _ _ sdoc ->
                             $(logDebug) ("Bogus output after log action has been reset: " <>
                             T.pack (showSDoc df sdoc)))))
     return v

-- | GHC gives unhelpful severity in the presence of
-- -fdefer-type-errors, so we try to reclaim error information by
-- doing dirty wirty parsing.
translateSeverity :: DynFlags -> Severity -> SDoc -> Severity
translateSeverity df sv msg =
  case sv of
    SevWarning
      | isError msgstr -> SevError
      | otherwise -> SevWarning
    s -> s
  where msgstr = showSDoc df msg


-- | Is the message actually an error?
isError :: [Char] -> Bool
isError s =
  isPrefixOf "No instance for "
             (trim s) ||
  isPrefixOf "Couldn't match "
             (trim s) ||
  isPrefixOf "Ambiguous "
             (trim s) ||
  isPrefixOf "Could not deduce "
             (trim s)
  where trim = unwords . words

-- | Print list of loaded imports.
loadedImports :: [ModuleName] -> [String]
loadedImports = map (\m -> "import " <> moduleNameString m)

-- | Make user flags, if HSENV is activated then use the
-- PACKAGE_DB_FOR_GHC environment variable for package flags.
makeUserFlags :: GhcMonad m => m [String]
makeUserFlags =
  do env <- liftIO getEnvironment
     case lookup "HSENV" env >>
          lookup "PACKAGE_DB_FOR_GHC" env of
       Just uflags -> return (words uflags)
       Nothing ->
         case lookup "GHC_PACKAGE_PATH" env of
           Just path ->
             return ["-hide-all-packages","-pkg-db=" <> path]
           Nothing -> return []
