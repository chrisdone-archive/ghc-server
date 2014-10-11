-- | Information about modules, identifiers, etc.

module GHC.Server.Controller.Info where

import GHC.Compat
import GHC.Server.Duplex
import GHC.Server.Model.Find
import GHC.Server.Types

import Data.Text (Text)

-- | Location of identifier at point.
locationAt :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns SrcSpan
locationAt fp ident sl sc el ec =
  do infos <- getModuleInfos
     result <- withGhc (findLoc infos fp ident sl sc el ec)
     case result of
       Left err -> error err
       Right sp -> return sp

-- | Type of identifier at point.
typeAt :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns Text
typeAt fp ident sl sc el ec =
  do infos <- getModuleInfos
     result <- withGhc (findType infos fp ident sl sc el ec)
     case result of
       Left err -> error err
       Right sp -> return sp

-- | Find uses.
usesAt :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns Text
usesAt fp ident sl sc el ec =
  do infos <- getModuleInfos
     result <- withGhc (findVar infos fp ident sl sc el ec)
     case result of
       Left err -> error err
       Right _ -> error "uses: Not implemented yet."
