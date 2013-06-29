-- | Client commands.

module Server.Commands where

import Server.Types
import Server.Import

import Data.Dynamic
import Data.List
import Data.Maybe
import GHC
import Outputable

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
                    mapM (fmap IIDecl . parseImportDecl)
                         (imports ++ loadedImports loaded)
                         >>= setContext
                    io (endResult results (LoadResult result)))
      Eval expr ->
        withGhc (do compiled <- dynCompileExpr ("show (" ++ expr ++ ")")
                    io (endResult results (EvalResult (dynString compiled))))
      TypeOf expr ->
        withGhc (do typ <- exprType expr
                    df <- getSessionDynFlags
                    io (endResult results (TypeResult (unlines (formatType df typ)))))
      KindOf expr ->
        withGhc (do typ <- typeKind True expr
                    df <- getSessionDynFlags
                    io (endResult results (KindResult (unlines (formatType df (snd typ))))))
      InfoOf ident ->
        withGhc (do names <- parseName ident
                    df <- getSessionDynFlags
                    infos <- fmap (concatMap (\(t,f,cs) -> sdoc df t : sdoc df f : map (sdoc df) cs)
                                   . catMaybes)
                                  (mapM getInfo names)
                    io (endResult results (InfoResult (unlines infos))))

  where imports = ["import Prelude"]
        loadedImports = map (\m -> "import " ++ moduleNameString m)
        unlines = intercalate "\n"

--------------------------------------------------------------------------------
-- GHC operations

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
