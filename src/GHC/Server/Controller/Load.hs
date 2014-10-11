{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Loading targets.

module GHC.Server.Controller.Load where

import           GHC.Compat
import           GHC.Server.Defaults
import           GHC.Server.Duplex
import           GHC.Server.Logging
import           GHC.Server.Model.Ghc
import           GHC.Server.Model.Info
import           GHC.Server.Types

import           Control.Arrow
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- | Load a module.
loadTarget :: Text -> Producer Msg (SuccessFlag,Integer)
loadTarget filepath =
  withGhc (do df <- getSessionDynFlags
              warnings <- liftIO (atomically (newTVar (0,0)))
              (result,loaded) <- withMessages (recordMessage warnings)
                                              doLoad
              case result of
                Succeeded ->
                  do var <- getModuleInfosVar
                     void (forkGhc (runLogging (collectInfo var loaded)))
                _ -> return ()
              count <- liftIO (atomically (readTVar warnings))
              return (result,snd count))
  where recordMessage warnings df sev sp doc =
          do send (Msg sev sp (T.pack (showSDoc df doc)))
             case sev of
               SevWarning ->
                 liftIO (atomically
                           (modifyTVar' warnings
                                        (second (+ 1))))
               SevError ->
                 liftIO (atomically
                           (modifyTVar' warnings
                                        (first (+ 1))))
               _ -> return ()
        doLoad =
          do target <- guessTarget (T.unpack filepath)
                                   Nothing
             setTargets [target]
             result <- load LoadAllTargets
             loaded <- getModuleGraph >>= filterM isLoaded . map ms_mod_name
             mapM parseImportDecl (necessaryImports <> loadedImports loaded) >>=
               setContext
             return (result,loaded)

-- | Collect type info data for the loaded modules.
collectInfo :: (GhcMonad m,MonadLogger m)
            => TVar (Map ModuleName ModInfo) -> [ModuleName] -> m ()
collectInfo var loaded =
  do ($(logDebug)
        ("Collecting module data for " <>
         T.pack (show (length loaded)) <>
         " modules ..."))
     forM_ loaded
           (\name ->
              do info <- getModInfo name
                 io (atomically
                       (modifyTVar var
                                   (M.insert name info))))
     $(logDebug) ("Done collecting module data.")
