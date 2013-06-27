-- | Handling the client's requests.

module Server.Client where

import Server.Types
import Server.Import

import Data.Dynamic
import GHC

-- | Call a command, return a result.
clientCall :: (Ghc () -> IO ()) -> Cmd -> Chan ResultType -> IO ()
clientCall withGhc cmd results =
  case cmd of
    Ping i -> do endResult results (Pong i)
    Eval expr ->
      withGhc (do compiled <- dynCompileExpr ("show (" ++ expr ++ ")")
                  liftIO (endResult results (EvalResult (dynString compiled))))
    LoadTarget string ->
      withGhc (do target <- guessTarget string Nothing
                  setTargets [target]
                  result <- load LoadAllTargets
                  loaded <- getModuleGraph >>= filterM isLoaded . map ms_mod_name
                  mapM (fmap IIDecl . parseImportDecl)
                       (imports ++ loadedImports loaded) >>= setContext
                  liftIO (endResult results (LoadResult result)))

  where imports = ["import Prelude"]
        loadedImports = map (\m -> "import " ++ moduleNameString m)

-- | Send an end result.
endResult :: Chan ResultType -> Result -> IO ()
endResult chan r = writeChan chan (EndResult r)

-- | Send a result of one or more results.
addResult :: Chan ResultType -> Result -> IO ()
addResult chan r = writeChan chan (Result r)

dynString :: Dynamic -> String
dynString d = fromDyn d ""
