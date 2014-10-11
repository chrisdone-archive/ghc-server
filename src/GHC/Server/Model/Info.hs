{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Get information on modules, identifiers, etc.

module GHC.Server.Model.Info (getModInfo) where

import           GHC.Compat
import           GHC.Server.Types

import qualified Data.Text as T
import           Control.Monad.Logger
import qualified Data.ByteString.Char8 as S8
import           Data.Generics (GenericQ, mkQ, extQ, gmapQ)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable

-- | Get info about the module: summary, types, etc.
getModInfo :: (MonadLogger m,GhcMonad m) => ModuleName -> m ModInfo
getModInfo name =
  do df <- getSessionDynFlags
     $(logDebug) ("Generating info for module " <> T.pack (showppr df name))
     m <- getModSummary name
     p <- parseModule m
     typechecked <- typecheckModule p
     allTypes <- processAllTypeCheckedModule typechecked
     let i = tm_checked_module_info typechecked
     return (ModInfo m allTypes i)

-- | Get ALL source spans in the module.
processAllTypeCheckedModule :: GhcMonad m
                            => TypecheckedModule -> m [SpanInfo]
processAllTypeCheckedModule tcm =
  do let tcs = tm_typechecked_source tcm
         bs =
           listifyAllSpans tcs :: [LHsBind Id]
         es =
           listifyAllSpans tcs :: [LHsExpr Id]
         ps =
           listifyAllSpans tcs :: [LPat Id]
     bts <- mapM (getTypeLHsBind tcm) bs
     ets <- mapM (getTypeLHsExpr tcm) es
     pts <- mapM (getTypeLPat tcm) ps
     doThatThing $ sortBy cmp $ catMaybes $
       concat [ets,bts,pts]
  where cmp (_,a,_) (_,b,_)
          | a `isSubspanOf` b = LT
          | b `isSubspanOf` a = GT
          | otherwise = EQ

#if __GLASGOW_HASKELL__ >= 706
toTup :: DynFlags -> (Maybe Id,SrcSpan, Type) -> Maybe SpanInfo
toTup dflags (n,spn,typ) =
  fmap (\s ->
          s {spaninfoVar = n
            ,spaninfoType =
               S8.pack (showppr dflags typ)})
       (getSrcSpan' spn)
#else
toTup :: DynFlags -> (Maybe Id, SrcSpan, Type) -> Maybe SpanInfo
toTup dflags (n,spn,typ) =
  fmap (\s ->
          s {spaninfoType = S8.pack (showppr dflags typ)
            ,spaninfoVar = n})
       (getSrcSpan' spn)
#endif

getSrcSpan' :: SrcSpan -> Maybe SpanInfo
getSrcSpan' (RealSrcSpan spn) =
  Just (SpanInfo (srcSpanStartLine spn)
                 (srcSpanStartCol spn - 1)
                 (srcSpanEndLine spn)
                 (srcSpanEndCol spn - 1)
                 mempty
                 Nothing)
getSrcSpan' _ = Nothing

getTypeLHsBind :: (GhcMonad m)
               => TypecheckedModule
               -> LHsBind Id
               -> m (Maybe (Maybe Id,SrcSpan,Type))
#if __GLASGOW_HASKELL__ >= 708
getTypeLHsBind _ (L spn FunBind{fun_id=pid, fun_matches = MG _ _ typ}) =
  return (Just (Just (unLoc pid),spn,typ))
#else
getTypeLHsBind _ (L spn FunBind{fun_id=pid, fun_matches = MatchGroup _ typ}) =
  return (Just (Just (unLoc pid),spn,typ))
#endif
getTypeLHsBind _ _ = return Nothing

#if __GLASGOW_HASKELL__ >= 708
getTypeLHsExpr :: (GhcMonad m)
               => TypecheckedModule
               -> LHsExpr Id
               -> m (Maybe (Maybe Id,SrcSpan,Type))
getTypeLHsExpr _ e =
  do hs_env <- getSession
     (_,mbe) <- liftIO (deSugarExpr hs_env e)
     case mbe of
       Nothing -> return Nothing
       Just expr ->
         return (Just (case unLoc e of
                         HsVar i -> Just i
                         _ -> Nothing
                      ,getLoc e
                      ,exprTypeCore expr))
#else
getTypeLHsExpr :: (GhcMonad m)
               => TypecheckedModule
               -> LHsExpr Id
               -> m (Maybe (Maybe Id,SrcSpan,Type))
getTypeLHsExpr tcm e = do
    hs_env <- getSession
    (_, mbe) <- liftIO $ deSugarExpr hs_env modu rn_env ty_env e
    return ()
    case mbe of
        Nothing -> return Nothing
        Just expr -> return $ Just (case unLoc e of
                                      HsVar i -> Just i
                                      _ -> Nothing,getLoc e, exprTypeCore expr)
    where
    modu = ms_mod $ pm_mod_summary $ tm_parsed_module tcm
    rn_env = tcg_rdr_env $ fst $ tm_internals_ tcm
    ty_env = tcg_type_env $ fst $ tm_internals_ tcm
#endif

getTypeLPat :: (GhcMonad m)
            => TypecheckedModule -> LPat Id -> m (Maybe (Maybe Id,SrcSpan,Type))
getTypeLPat _ (L spn pat) =
  return (Just (Nothing,spn,hsPatType pat))

-- | Get ALL source spans in the source.
listifyAllSpans :: Typeable a => TypecheckedSource -> [Located a]
listifyAllSpans tcs = listifyStaged TypeChecker p tcs
    where
    p (L spn _) = isGoodSrcSpan spn

listifyStaged :: Typeable r => Stage -> (r -> Bool) -> GenericQ [r]
listifyStaged s p = everythingStaged s (++) [] ([] `mkQ` (\x -> [x | p x]))

------------------------------------------------------------------------------
-- The following was taken from 'ghc-syb-utils'
--
-- ghc-syb-utils:
--     https://github.com/nominolo/ghc-syb

-- | Ghc Ast types tend to have undefined holes, to be filled
--   by later compiler phases. We tag Asts with their source,
--   so that we can avoid such holes based on who generated the Asts.
data Stage = Parser | Renamer | TypeChecker deriving (Eq,Ord,Show)

-- | Like 'everything', but avoid known potholes, based on the 'Stage' that
--   generated the Ast.
everythingStaged :: Stage -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
everythingStaged stage k z f x
  | (const False `extQ` postTcType `extQ` fixity `extQ` nameSet) x = z
  | otherwise = foldl k (f x) (gmapQ (everythingStaged stage k z f) x)
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
        postTcType = const (stage<TypeChecker)                 :: PostTcType -> Bool
        fixity     = const (stage<Renamer)                     :: Fixity -> Bool

-- | I prefer to separate vulgar CPP nonsense outside of respectable functions.
doThatThing ::  GhcMonad m => [(Maybe Id, SrcSpan, Type)] -> m [SpanInfo]
doThatThing x = do
    dflags <- getSessionDynFlags
    return (mapMaybe (toTup dflags) x)
