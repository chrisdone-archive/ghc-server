{-# LANGUAGE ScopedTypeVariables #-}
-- | Client commands.

module Server.Commands where

import Server.Types
import Server.Import

import Data.Dynamic
import Data.List
import Data.Maybe
import GHC.Compat

-- | Call a command, return a result.
clientCall :: (Ghc () -> IO ()) -> Cmd -> Chan ResultType -> IO ()
clientCall withGhc cmd results =
 do case cmd of
      Ping i ->
        endResult results (Pong i)
      LoadTarget string ->
        withGhc (do target <- guessTarget string Nothing
                    setTargets [target]
                    result <- load LoadAllTargets
                    loaded <- getModuleGraph >>= filterM isLoaded . map ms_mod_name
                    mapM parseImportDecl
                         (imports ++ loadedImports loaded)
                         >>= setContext
                    io (endResult results (LoadResult result)))
      Eval expr ->
        withGhc (doExpr results expr)
      TypeOf expr ->
        withGhc (do typ <- addLogsToResults results
                                            (exprType expr)
                    df <- getSessionDynFlags
                    io (endResult results (TypeResult (unlines (formatType df typ)))))
      KindOf expr ->
        withGhc (do typ <- addLogsToResults results
                                            (typeKind expr)
                    df <- getSessionDynFlags
                    io (endResult results (KindResult (unlines (formatType df typ)))))
      InfoOf ident ->
        withGhc (do names <- parseName ident
                    df <- getSessionDynFlags
                    infos <- fmap (concatMap (\(t,f,cs) -> sdoc df t : sdoc df f : map (sdoc df) cs)
                                   . catMaybes)
                                  (mapM getInfo names)
                    io (endResult results (InfoResult (unlines infos))))
      Set flag ->
        withGhc (do setFlag flag
                    io (endResult results Unit))
      PackageConf pkgconf ->
        withGhc (do setFlag ("-package-conf=" ++ pkgconf)
                    df <- getSessionDynFlags
                    (dflags,_pkgs) <- io (initPackages df)
                    setSessionDynFlags dflags
                    io (endResult results Unit))

  where imports = ["import Prelude"]
        loadedImports = map (\m -> "import " ++ moduleNameString m)
        unlines = intercalate "\n"

doExpr results expr =
  do dflags <- getSessionDynFlags
     typeResult <- gtry (exprType expr)
     case typeResult of
       Left (err :: SomeException) ->
         do enames <- gtry (runDecls expr)
            case enames of
              Right names ->
                io (endResult results (DeclResult (map (sdoc dflags) names)))
              Left (err :: SomeException) ->
                tryEvaluating results dflags expr
       Right ty ->
         do io (addResult results (TypeResult (sdoc dflags ty)))
            logger (Debug ("Got type: " ++ sdoc dflags ty))
            tryEvaluating results dflags expr

tryEvaluating results dflags expr =
  do dyn <- addLogsToResults results (tryDynCompileExpr (exprPure expr))
     case fmap fromDynamic dyn of
       Right (Just str) ->
         do logger (Debug ("Running showable expression value..."))
            io (endResult results (EvalResult str))
       _ ->
         do dyn <- addLogsToResults results (tryDynCompileExpr (exprIOShowable expr))
            case fmap fromDynamic dyn of
              Right (Just action) ->
                do logger (Debug ("Running IO action returning Show instance..."))
                   result <- liftIO action
                   io (endResult results (EvalResult result))
              _ ->
                do dyn <- addLogsToResults results (tryDynCompileExpr (exprIOUnknown expr))
                   case fmap fromDynamic dyn of
                     Right (Just action) ->
                       do logger (Debug ("Running IO action returning unshowable value..."))
                          () <- liftIO action
                          return ()
                     _ ->
                       runStatement results expr

runStatement results expr =
  do logger (Debug ("runStmt"))
     result <- runStmt expr RunToCompletion
     logger (Debug ("Got result."))
     dflags <- getSessionDynFlags
     case result of
       RunOk names -> io (endResult results (DeclResult (map (sdoc dflags) names)))
       RunException e -> throw e
       RunBreak{} -> return ()

-- | Make an expression for evaluating pure expressions and printing
-- the result.
exprPure :: String -> String
exprPure expr =
  concat
    [before
    ,(let ls = lines expr
      in intercalate "\n" (take 1 ls ++
                           map (replicate (length before) ' ' ++)
                               (drop 1 ls)))
    ,")"]
  where before = "Prelude.show ("

-- | Make an expression for running IO actions and printing the
-- result.
exprIOShowable :: String -> String
exprIOShowable expr =
  concat
    [before
    ,(let ls = lines expr
      in intercalate "\n" (take 1 ls ++
                           map (replicate (length before) ' ' ++)
                               (drop 1 ls)))
    ,") >>= return . show"]
  where before = "("

-- | Make an expression for running IO actions and printing the
-- result.
exprIOUnknown :: String -> String
exprIOUnknown expr =
  concat
    [before
    ,(let ls = lines expr
      in intercalate "\n" (take 1 ls ++
                           map (replicate (length before) ' ' ++)
                               (drop 1 ls)))
    ,") >> return ()"]
  where before = "("

-- | Try to compile an expression.
tryDynCompileExpr :: GhcMonad m => String -> m (Either SomeException Dynamic)
tryDynCompileExpr expr =
  gcatch (fmap Right (dynCompileExpr expr))
         (\(e::SomeException) -> return (Left e))

--------------------------------------------------------------------------------
-- GHC operations

setFlag flag = do
  df <- getSessionDynFlags
  (dflags,_,_) <- parseDynamicFlags df (map (mkGeneralLocated "flag") [flag])
  void (setSessionDynFlags dflags)

-- | Add any GHC logs to the given result channel.
addLogsToResults :: GhcMonad m => Chan ResultType -> m b -> m b
addLogsToResults results m = do
  dflags <- getSessionDynFlags
  setLogAction addLog
  result <- m
  setSessionDynFlags dflags
  return result

  where addLog dflags severity span _style msg =
          addResult results (LogResult severity span (showSDoc dflags msg))


sdoc :: Outputable a => DynFlags -> a -> String
sdoc dflags = showSDocForUser dflags neverQualify . ppr

formatType :: DynFlags -> Type -> [String]
formatType dflags = lines . sdoc dflags . snd . splitForAllTys

dynString :: Dynamic -> String
dynString d = fromDyn d ""

--------------------------------------------------------------------------------
-- Command replying

-- | Send an end result.
endResult :: Chan ResultType -> Result -> IO ()
endResult chan r = writeChan chan (EndResult r)

-- | Send a result of one or more results.
addResult :: Chan ResultType -> Result -> IO ()
addResult chan r = writeChan chan (Result r)
