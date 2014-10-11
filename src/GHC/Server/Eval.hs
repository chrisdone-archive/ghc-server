{-# LANGUAGE ScopedTypeVariables #-}

-- | Run eval.

module GHC.Server.Eval where

import           GHC.Compat
import           GHC.Server.Types

import           Data.Text (Text)
import qualified Data.Text as T

-- | Try to run the expression as an import line:
--
-- import F
--
-- Or as a declaration:
--
-- data X = X
--
-- Otherwise try evaluating it as an expression.
tryImportOrDecls :: (GhcMonad m)
                 => Text -> m EvalResult
tryImportOrDecls e =
  do dflags <- getSessionDynFlags
     result <- gtry (parseImportDecl (T.unpack e))
     case result of
       Right imp ->
         do addToContext imp
            ctx <- getContext
            return (NewContext (map (showppr dflags) ctx))
       Left (ex :: SomeException) -> throw ex
