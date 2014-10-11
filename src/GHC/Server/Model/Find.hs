{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

-- | Finding identifiers, types, etc.

module GHC.Server.Model.Find where

import           GHC.Compat
import           GHC.Server.Types
import           System.Directory

import           Control.Monad.Logger
import           Data.ByteString (ByteString)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Try to find the location of the given identifier at the given
-- position in the module, without looking at any external place.
findVar :: (GhcMonad m,MonadLogger m)
        => Map ModuleName ModInfo
        -> FilePath
        -> Text
        -> Int
        -> Int
        -> Int
        -> Int
        -> m (Either String Var)
findVar infos fp _string sl sc el ec =
  do mname <- guessModule infos fp
     case mname of
       Nothing ->
         $(logDebug) (("Couldn't guess the module name."))
       _ -> return ()
     case mname >>=
          flip M.lookup infos of
       Nothing ->
         return (Left ("No module info for the current file! Try loading it?"))
       Just info ->
         do d <- getSessionDynFlags
            case resolveName (modinfoSpans info)
                             sl
                             sc
                             el
                             ec of
              Nothing ->
                return (Left "Couldn't resolve name.")
              Just name ->
                case getSrcSpan name of
                  UnhelpfulSpan{} ->
                    return (Left ("Found a name, but no location information. The module is: " ++
                                  maybe "<unknown>"
                                        (showppr d . moduleName)
                                        (nameModule_maybe (getName name))))
                  _ -> return (Right name)

-- | Try to find the location of the given identifier at the given
-- position in the module.
findLoc :: (GhcMonad m,MonadLogger m)
        => Map ModuleName ModInfo
        -> FilePath
        -> Text
        -> Int
        -> Int
        -> Int
        -> Int
        -> m (Either String SrcSpan)
findLoc infos fp string sl sc el ec =
  do mname <- guessModule infos fp
     case mname of
       Nothing ->
         $(logDebug) (("Couldn't guess the module name."))
       _ -> return ()
     case mname >>=
          flip M.lookup infos of
       Nothing ->
         return (Left ("No module info for the current file! Try loading it?"))
       Just info ->
         do mname' <- findName infos info string sl sc el ec
            d <- getSessionDynFlags
            case mname' of
              Left reason -> return (Left reason)
              Right name ->
                case getSrcSpan name of
                  UnhelpfulSpan{} ->
                    return (Left ("Found a name, but no location information. The module is: " ++
                                  maybe "<unknown>"
                                        (showppr d . moduleName)
                                        (nameModule_maybe name)))
                  span' -> return (Right span')

-- | Try to resolve the name located at the given position, or
-- otherwise resolve based on the current module's scope.
findName :: GhcMonad m
         => Map ModuleName ModInfo
         -> ModInfo
         -> Text
         -> Int
         -> Int
         -> Int
         -> Int
         -> m (Either String Name)
findName infos mi string sl sc el ec =
  case resolveName (modinfoSpans mi)
                   sl
                   sc
                   el
                   ec of
    Nothing -> tryExternalModuleResolution
    Just name ->
      case getSrcSpan name of
        UnhelpfulSpan{} -> tryExternalModuleResolution
        _ -> return (Right (getName name))
  where tryExternalModuleResolution =
          case find (matchName (T.unpack string))
                    (fromMaybe [] (modInfoTopLevelScope (modinfoInfo mi))) of
            Nothing ->
              return (Left "Couldn't resolve to any modules.")
            Just imported -> resolveNameFromModule infos imported
        matchName :: String -> Name -> Bool
        matchName str name =
          str ==
          occNameString (getOccName name)

-- | Try to resolve the name from another (loaded) module's exports.
resolveNameFromModule :: GhcMonad m
                      => Map ModuleName ModInfo
                      -> Name
                      -> m (Either String Name)
resolveNameFromModule infos name =
  do d <- getSessionDynFlags
     case nameModule_maybe name of
       Nothing ->
         return (Left ("No module for " ++
                       showppr d name))
       Just modL ->
         do case M.lookup (moduleName modL) infos of
              Nothing ->
                do (return (Left ("No locally loaded module for " ++
                                  showppr d modL ++
                                  ". It's in this package: " ++
                                  showppr d (modulePackageId modL))))
              Just info ->
                case find (matchName name)
                          (modInfoExports (modinfoInfo info)) of
                  Just name' ->
                    return (Right name')
                  Nothing ->
                    return (Left "No matching export in any local modules.")
  where matchName :: Name -> Name -> Bool
        matchName x y =
          occNameString (getOccName x) ==
          occNameString (getOccName y)

-- | Try to resolve the type display from the given span.
resolveName :: [SpanInfo] -> Int -> Int -> Int -> Int -> Maybe Var
resolveName spans' sl sc el ec =
  listToMaybe (mapMaybe spaninfoVar (filter inside (reverse spans')))
  where inside (SpanInfo sl' sc' el' ec' _ _) =
          ((sl' == sl && sc' >= sc) || (sl' > sl)) &&
          ((el' == el && ec' <= ec) || (el' < el))

-- | Try to find the type of the given span.
findType :: GhcMonad m
         => Map ModuleName ModInfo
         -> FilePath
         -> Text
         -> Int
         -> Int
         -> Int
         -> Int
         -> m (Either String Text)
findType infos fp string sl sc el ec =
  do mname <- guessModule infos fp
     case mname >>=
          flip M.lookup infos of
       Nothing ->
         return (Left ("Didn't find any module info. Is this module loaded?"))
       Just info ->
         do let !mty =
                  resolveType (modinfoSpans info)
                              sl
                              sc
                              el
                              ec
            case mty of
              Just ty ->
                return (Right (T.decodeUtf8 ty))
              Nothing ->
                do d <- getSessionDynFlags
                   fmap (Right . T.pack . showppr d)
                        (exprType (T.unpack string))

-- | Try to resolve the type display from the given span.
resolveType :: [SpanInfo] -> Int -> Int -> Int -> Int -> Maybe ByteString
resolveType spans' sl sc el ec =
  fmap spaninfoType (find inside (reverse spans'))
  where inside (SpanInfo sl' sc' el' ec' _ _) =
          ((sl' == sl && sc' >= sc) || (sl' > sl)) &&
          ((el' == el && ec' <= ec) || (el' < el))

-- | Guess a module name from a file path.
guessModule :: GhcMonad m
            => Map ModuleName ModInfo -> FilePath -> m (Maybe ModuleName)
guessModule infos path =
  do fp <- liftIO (makeRelativeToCurrentDirectory path)
     target <- guessTarget fp Nothing
     case targetId target of
       TargetModule mn -> return (Just mn)
       _ ->
         case find ((Just fp ==) .
                    ml_hs_file .
                    ms_location .
                    modinfoSummary .
                    snd)
                   (M.toList infos) of
           Just (mn,_) -> return (Just mn)
           Nothing -> return Nothing
