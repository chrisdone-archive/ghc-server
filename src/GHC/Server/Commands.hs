{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Client commands.

module GHC.Server.Commands (clientCall) where

import GHC.Server.IO
import GHC.Server.Import

import Data.Dynamic
import Data.List
import Data.Maybe
import GHC.Compat

-- | Call a command, return a result.
clientCall :: (Ghc () -> IO ()) -> Cmd -> Chan ResultType -> IO ()
clientCall runGhc cmd results =
 do case cmd of
      Ping i ->
        endResult results (Pong i)
      LoadTarget string ->
        withGhc (do target <- guessTarget string Nothing
                    setTargets [target]
                    result <- load LoadAllTargets
                    loaded <- getModuleGraph >>= filterM isLoaded . map ms_mod_name
                    mapM parseImportDecl
                         (necessaryImports ++ loadedImports loaded)
                         >>= setContext
                    io (endResult results (LoadResult result)))
      Eval expr ->
        withGhc (do tryImportOrDecls results expr)
      TypeOf expr ->
        withGhc (do typ <- exprType expr
                    df <- getSessionDynFlags
                    io (endResult results (TypeResult (unlines (formatType df typ)))))
      KindOf expr ->
        withGhc (do typ <- typeKind expr
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
        runGhc (do setFlag flag
                   io (endResult results Unit))
      PackageConf pkgconf ->
        runGhc (do setFlag ("-package-conf=" ++ pkgconf)
                   df <- getSessionDynFlags
                   (dflags,_pkgs) <- io (initPackages df)
                   setSessionDynFlags dflags
                   io (endResult results Unit))
  where loadedImports = map (\m -> "import " ++ moduleNameString m)
        unlines = intercalate "\n"
        withGhc = runGhc . addLogsToResults results

-- | Try to run the expression as an import line:
--
-- import F
--
-- Or as a declaration:
--
-- data X = X
--
-- Otherwise try evaluating it as an expression.
tryImportOrDecls :: Chan ResultType -> String -> Ghc ()
tryImportOrDecls results expr =
  do dflags <- getSessionDynFlags
     result <- gtry (parseImportDecl expr)
     case result of
       Right imp ->
         do addToContext imp
            ctx <- getContext
            io (addResult results (EvalImport (map (sdoc dflags) ctx)))
       Left (_ :: SomeException) ->
         do typeResult <- gtry (exprType expr)
            case typeResult of
              Left (err :: SomeException) ->
                do enames <- gtry (runDecls expr)
                   case enames of
                     Right names ->
                       io (endResult results (DeclResult (map (sdoc dflags) names)))
                     Left (err :: SomeException) ->
                       tryEvaluating "" results expr
              Right ty ->
                do io (addResult results (TypeResult (sdoc dflags ty)))
                   logger (Debug ("Got type: " ++ sdoc dflags ty))
                   tryEvaluating (sdoc dflags ty) results expr

-- | Try evaluating it as an expression.
--
-- 23 * 53
--
-- Otherwise try running it as an IO action.
tryEvaluating :: String -> Chan ResultType -> String -> Ghc ()
tryEvaluating typ results expr =
  do dyn <- tryDynCompileExpr (exprPure expr)
     case fmap fromDynamic dyn of
       Right (Just str) ->
         do logger (Debug ("Running showable expression value..."))
            io (endResult results (EvalResult str))
       _ ->
         tryRunning typ results expr

-- | Try running it as an IO action.
--
-- getLine
-- putStrLn \"Hello!\"
--
-- Otherwise try running it as an interactive statement.
tryRunning :: String -> Chan ResultType -> String -> Ghc ()
tryRunning typ results stmt =
  do dyn <- tryDynCompileExpr (exprIOShowable stmt)
     case fmap fromDynamic dyn of
       Right (Just (constrain -> action)) ->
         do logger (Debug ("Running IO action returning Show instance..."))
            result <- liftIO (action handleStdin)
            if typ == "IO ()"
               then io (endResult results Unit)
               else io (endResult results (EvalResult result))
       _ ->
         do dyn <- tryDynCompileExpr (exprIOUnknown stmt)
            case fmap fromDynamic dyn of
              Right (Just (constrain -> action)) ->
                do logger (Debug ("Running IO action returning unshowable value..."))
                   () <- liftIO (action handleStdin)
                   io (endResult results Unit)
              Left e ->
                do io (putStrLn ("Error: " ++ show e))
                   runStatement results stmt
  where constrain action =
          asTypeOf action (runIO (return undefined))
        handleStdin bytes =
          addResult results (EvalStdout bytes)

-- | Try running it as an interactive statement.
--
-- let x = 123
-- x <- return 123
--
-- Otherwise give up.
runStatement :: Chan ResultType -> String -> Ghc ()
runStatement results stmt =
  do io (putStrLn "Running statement ...")
     result <- gtry (runStmt stmt RunToCompletion)
     dflags <- getSessionDynFlags
     case result of
       Left (SomeException e :: SomeException) ->
         throw e
       Right r ->
         case r of
           RunOk names ->
             io (endResult results (DeclResult (map (sdoc dflags) names)))
           RunException e ->
             throw e
           RunBreak{} ->
             return ()

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
    ,") >>= return . show)"]
  where before = "GHC.Server.IO.runIO (("

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
    ,") >> return ())"]
  where before = "GHC.Server.IO.runIO (("

-- | Try to compile an expression.
tryDynCompileExpr :: GhcMonad m => String -> m (Either SomeException Dynamic)
tryDynCompileExpr expr =
  gcatch (fmap Right (dynCompileExpr expr))
         (\(e::SomeException) -> return (Left e))

--------------------------------------------------------------------------------
-- GHC operations

-- | Apply a flag.
setFlag :: GhcMonad m => String -> m ()
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

-- | Render an outputable thing.
sdoc :: Outputable a => DynFlags -> a -> String
sdoc dflags = showSDocForUser dflags neverQualify . ppr

-- | Pretty print a type.
formatType :: DynFlags -> Type -> [String]
formatType dflags = lines . sdoc dflags . snd . splitForAllTys

--------------------------------------------------------------------------------
-- Command replying

-- | Send an end result.
endResult :: Chan ResultType -> Result -> IO ()
endResult chan r = writeChan chan (EndResult r)

-- | Send a result of one or more results.
addResult :: Chan ResultType -> Result -> IO ()
addResult chan r = writeChan chan (Result r)
