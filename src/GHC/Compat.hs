{-# LANGUAGE CPP #-}

-- | Compatibility layer for GHC.
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
  ,parseImportDecl
  ,typeKind
  ,setContext)
  where

import BasicTypes hiding (Version)
import DynFlags
import FastString
import GHC
  hiding (parseImportDecl
         ,typeKind
         ,setContext)
import qualified GHC
import GHC.Paths
import GhcMonad
import MonadUtils
import Outputable
import Packages
import SrcLoc

-- | Wraps 'typeKind'.
typeKind :: GhcMonad m => String -> m Kind
#if __GLASGOW_HASKELL__ == 702
typeKind expr = GHC.typeKind expr
#endif
#if __GLASGOW_HASKELL__ == 704
typeKind expr = fmap snd (GHC.typeKind True expr)
#endif

-- | Wraps 'parseImportDecl'.
parseImportDecl :: GhcMonad m => String -> m (ImportDecl RdrName)
#if __GLASGOW_HASKELL__ == 702
parseImportDecl = GHC.parseImportDecl
#endif
#if __GLASGOW_HASKELL__ == 704
parseImportDecl = GHC.parseImportDecl
#endif

-- | Wraps 'setContext'.
setContext :: GhcMonad m => [ImportDecl RdrName] -> m ()
#if __GLASGOW_HASKELL__ == 702
setContext = GHC.setContext []
#endif
#if __GLASGOW_HASKELL__ == 704
setContext = GHC.setContext . map IIDecl
#endif

#if __GLASGOW_HASKELL__ == 702
instance Show SrcSpan where show _ = "SrcSpan"
#endif
