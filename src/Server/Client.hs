-- | Handling the client's requests.

module Server.Client where

import Server.Types

import GHC
import Data.Dynamic

-- | Call a command, return a result.
clientCall :: Cmd -> Ghc Result
clientCall cmd =
  case cmd of
    Ping -> return Pong
    Eval expr ->
      do compiled <- dynCompileExpr expr
         return (EvalResult (defaultProcessOutput compiled))

defaultProcessOutput :: Dynamic -> String
defaultProcessOutput d = show d -- fromDyn d ""
