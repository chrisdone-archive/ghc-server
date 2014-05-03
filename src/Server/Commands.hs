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
      Ping i -> do endResult results (Pong i)
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
        withGhc (do let before = "show ("
                        expr' = before ++
                                       (let ls = lines expr
                                        in unlines (take 1 ls ++
                                                    map (replicate (length before) ' ' ++)
                                                        (drop 1 ls)))
                                       ++ ")"
                    logger (Debug ("Evaluating:\n" ++ expr'))
                    compiled <- addLogsToResults results
                                                 (dynCompileExpr expr')
                    io (endResult results (EvalResult (dynString compiled))))
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
  setSessionDynFlags dflags { log_action = addLog }
  result <- m
  setSessionDynFlags dflags
  return result

  where addLog severity span _style msg =
          addResult results (LogResult severity span (showSDoc msg))


sdoc :: Outputable a => DynFlags -> a -> String
sdoc dflags = showSDocForUser neverQualify . ppr

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
