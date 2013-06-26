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
    LoadFile file ->
      withGhc (do target <- guessTarget file Nothing
                  setTargets [target]
                  result <- load LoadAllTargets
                  mapM (fmap IIDecl . parseImportDecl) (imports ++ ["import X"]) >>= setContext
                  liftIO (endResult results (LoadResult result)))

  where imports = ["import Prelude"]

-- | Send an end result.
endResult :: Chan ResultType -> Result -> IO ()
endResult chan r = writeChan chan (EndResult r)

-- | Send a result of one or more results.
addResult :: Chan ResultType -> Result -> IO ()
addResult chan r = writeChan chan (Result r)

dynString :: Dynamic -> String
dynString d = fromDyn d ""
