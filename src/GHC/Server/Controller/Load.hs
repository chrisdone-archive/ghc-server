{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Loading targets.

module GHC.Server.Controller.Load where

import           GHC.Compat
import           GHC.Server.Defaults
import           GHC.Server.Duplex
import           GHC.Server.Model.Ghc
import           GHC.Server.Model.Info
import           GHC.Server.Types

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- | Load a module.
loadTarget :: Text -> Producer Msg (SuccessFlag,Integer)
loadTarget filepath =
  withGhc (do df <- getSessionDynFlags
              warnings <- liftIO (atomically (newTVar 0))
              result <- withMessages (recordMessage warnings df)
                                     doLoad
              count <- liftIO (atomically (readTVar warnings))
              return (result,count))
  where recordMessage warnings df =
          \sev sp doc ->
            do send (Msg sev sp (T.pack (showSDoc df doc)))
               case sev of
                 SevWarning ->
                   liftIO (atomically (modifyTVar' warnings (+ 1)))
                 _ -> return ()
        doLoad =
          do target <- guessTarget (T.unpack filepath)
                                   Nothing
             setTargets [target]
             result <- load LoadAllTargets
             loaded <- getModuleGraph >>= filterM isLoaded . map ms_mod_name
             mapM parseImportDecl (necessaryImports <> loadedImports loaded) >>=
               setContext
             case result of
               Succeeded -> collectInfo loaded
               _ -> return ()
             return result

-- | Collect type info data for the loaded modules.
collectInfo :: (MonadDuplex i o m,GhcMonad m)
            => [ModuleName] -> m ()
collectInfo loaded =
  do ($(logDebug)
        ("Collecting module data for " <>
         T.pack (show (length loaded)) <>
         " modules ..."))
     forM_ loaded
           (\name ->
              do info <- getModInfo name
                 var <- asks (stateModuleInfos . duplexState)
                 io (atomically
                       (modifyTVar var
                                   (M.insert name info))))
     $(logDebug) ("Done collecting module data.")
