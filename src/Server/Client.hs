-- | Handling the client's requests.

module Server.Client where

import Server.Types
import Server.Import

import GHC

-- | Call a command, return a result.
clientCall :: (Ghc () -> IO ()) -> Cmd -> Chan ResultType -> IO ()
clientCall withGhc cmd results =
  case cmd of
    Ping -> do endResult results Pong
    Eval expr ->
      withGhc (do compiled <- dynCompileExpr expr
                  liftIO (endResult results (EvalResult (show compiled))))

-- | Send an end result.
endResult :: Chan ResultType -> Result -> IO ()
endResult chan r = writeChan chan (EndResult r)

-- | Send a result of one or more results.
addResult :: Chan ResultType -> Result -> IO ()
addResult chan r = writeChan chan (Result r)
