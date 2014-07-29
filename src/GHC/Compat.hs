{-# LANGUAGE CPP #-}

-- | Compatibility layer for GHC. Supports GHC 7.4, 7.6, 7.8.
--
-- None of the ghc-server project should import from the GHC API
-- directly, it should import via this layer. It exports everything
-- from the GHC API that is needed. Ideally, only this module will use
-- CPP, too.
--
-- Some symbols are not exported, usurped by compatible
-- re-definitions. These compatibility wrappers are added on a
-- case-by-case basis. Otherwise, everything is re-exported.
--
-- Each function has a type signature. Under each type signature lies
-- an implementation dependent upon a specific major GHC version. When
-- a new GHC version is added to the test builds, a new #if section
-- will needed to be added for that specific version. If not, there
-- will be a build error. This helps to ensure specific versions are
-- dealt with.

module GHC.Compat
  (module GHC
  ,module GHC.Paths
  ,module Outputable
  ,module Packages
  ,module BasicTypes
  ,module DynFlags
  ,module GhcMonad
  ,module SrcLoc
  ,module FastString
  ,module MonadUtils
  ,module Exception
  ,parseImportDecl
  ,typeKind
  ,setContext
  ,defaultErrorHandler
  ,showSDocForUser
  ,setLogAction
  ,showSDoc
  ,getInfo
  ,addToContext)
  where

import           BasicTypes hiding (Version)
import qualified Control.Monad.Trans as Trans
import qualified DynFlags
import           ErrUtils
import           Exception
import           FastString
import qualified GHC
import           GHC.Paths
import           GhcMonad
import           MonadUtils
import qualified Outputable
import           Packages
import           SrcLoc
import           System.IO

import           DynFlags
  hiding (LogAction)

import           Outputable
  hiding (showSDocForUser
         ,showSDoc)

import           GHC
  hiding (parseImportDecl
         ,typeKind
         ,setContext
         ,defaultErrorHandler
         ,getInfo)

-- | Wraps 'GHC.typeKind'.
typeKind :: GhcMonad m => String -> m Kind
#if __GLASGOW_HASKELL__ == 704
typeKind expr = fmap snd (GHC.typeKind True expr)
#endif
#if __GLASGOW_HASKELL__ == 706
typeKind expr = fmap snd (GHC.typeKind True expr)
#endif
#if __GLASGOW_HASKELL__ == 708
typeKind expr = fmap snd (GHC.typeKind True expr)
#endif

-- | Wraps 'GHC.parseImportDecl'.
parseImportDecl :: GhcMonad m => String -> m (ImportDecl RdrName)
#if __GLASGOW_HASKELL__ == 704
parseImportDecl = GHC.parseImportDecl
#endif
#if __GLASGOW_HASKELL__ == 706
parseImportDecl = GHC.parseImportDecl
#endif
#if __GLASGOW_HASKELL__ == 708
parseImportDecl = GHC.parseImportDecl
#endif

-- | Wraps 'GHC.setContext'.
setContext :: GhcMonad m => [ImportDecl RdrName] -> m ()
#if __GLASGOW_HASKELL__ == 704
setContext = GHC.setContext . map IIDecl
#endif
#if __GLASGOW_HASKELL__ == 706
setContext = GHC.setContext . map IIDecl
#endif
#if __GLASGOW_HASKELL__ == 708
setContext = GHC.setContext . map IIDecl
#endif

-- | Add an import declaration to the context with `setContext`.
addToContext :: GhcMonad m => ImportDecl RdrName -> m ()
#if __GLASGOW_HASKELL__ == 704
addToContext i =
  do ctx <- getContext
     GHC.setContext (IIDecl i : ctx)
#endif
#if __GLASGOW_HASKELL__ == 706
addToContext i =
  do ctx <- getContext
     GHC.setContext (IIDecl i : ctx)
#endif
#if __GLASGOW_HASKELL__ == 708
addToContext i =
  do ctx <- getContext
     GHC.setContext (IIDecl i : ctx)
#endif

-- | Wraps 'GHC.defaultErrorHandler'.
defaultErrorHandler :: (MonadIO m,ExceptionMonad m) => m a -> m a
#if __GLASGOW_HASKELL__ == 704
defaultErrorHandler = GHC.defaultErrorHandler defaultLogAction
#endif
#if __GLASGOW_HASKELL__ == 706
defaultErrorHandler = GHC.defaultErrorHandler putStrLn (FlushOut (hFlush stdout))
#endif
#if __GLASGOW_HASKELL__ == 708
defaultErrorHandler = GHC.defaultErrorHandler putStrLn (FlushOut (hFlush stdout))
#endif

-- | Wraps 'Outputable.showSDocForUser'.
showSDocForUser :: DynFlags -> PrintUnqualified -> SDoc -> String
#if __GLASGOW_HASKELL__ == 704
showSDocForUser _ = Outputable.showSDocForUser
#endif
#if __GLASGOW_HASKELL__ == 706
showSDocForUser = Outputable.showSDocForUser
#endif
#if __GLASGOW_HASKELL__ == 708
showSDocForUser = Outputable.showSDocForUser
#endif

#if __GLASGOW_HASKELL__ == 704
type LogAction = DynFlags -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
#endif
#if __GLASGOW_HASKELL__ == 706
type LogAction = DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
#endif
#if __GLASGOW_HASKELL__ == 708
type LogAction = DynFlags -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
#endif

-- | Sets the log action for the session.
setLogAction :: GhcMonad m => LogAction -> m ()
#if __GLASGOW_HASKELL__ == 704
setLogAction logger =
  do dflags <- getSessionDynFlags
     setSessionDynFlags dflags { log_action = logger dflags }
     return ()
#endif
#if __GLASGOW_HASKELL__ == 706
setLogAction logger =
  do dflags <- getSessionDynFlags
     setSessionDynFlags dflags { log_action = logger }
     return ()
#endif
#if __GLASGOW_HASKELL__ == 708
setLogAction logger =
  do dflags <- getSessionDynFlags
     setSessionDynFlags dflags { log_action = logger }
     return ()
#endif

-- | Wraps 'Outputable.showSDoc'.
showSDoc :: DynFlags -> SDoc -> String
#if __GLASGOW_HASKELL__ == 704
showSDoc _ = Outputable.showSDoc
#endif
#if __GLASGOW_HASKELL__ == 706
showSDoc = Outputable.showSDoc
#endif
#if __GLASGOW_HASKELL__ == 708
showSDoc = Outputable.showSDoc
#endif

-- | An instance of a class.
#if __GLASGOW_HASKELL__ == 704
type SomeInstance = Instance
#endif
#if __GLASGOW_HASKELL__ == 706
type SomeInstance = ClsInst
#endif
#if __GLASGOW_HASKELL__ == 708
type SomeInstance = ClsInst
#endif

-- | Wraps 'GHC.getInfo'.
getInfo :: GhcMonad m => Name -> m (Maybe (TyThing, Fixity, [SomeInstance]))
#if __GLASGOW_HASKELL__ == 702
getInfo = GHC.getInfo
#endif
#if __GLASGOW_HASKELL__ == 704
getInfo = GHC.getInfo
#endif
#if __GLASGOW_HASKELL__ == 706
getInfo = GHC.getInfo
#endif
#if __GLASGOW_HASKELL__ == 708
getInfo = fmap (fmap (\(a,b,c,d) -> (a,b,c))) . GHC.getInfo False
#endif

-- Missing instances

#if __GLASGOW_HASKELL__ == 704
instance Trans.MonadIO Ghc where liftIO = GhcMonad.liftIO
#endif
#if __GLASGOW_HASKELL__ == 706
instance Trans.MonadIO Ghc where liftIO = GhcMonad.liftIO
#endif
