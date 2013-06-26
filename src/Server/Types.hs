{-# LANGUAGE OverloadedStrings #-}

module Server.Types where

import           BasicTypes
import           Control.Concurrent
import           Control.Monad.Trans
import qualified Data.AttoLisp as L
import           Data.Attoparsec.Number (Number(I))
import qualified Data.Text as T
import           GhcMonad

-- | A log type.
data Log
  = Debug String
  | Notice String
  | Error String
  | Fatal String
  deriving (Show)

-- | GHC has its own MonadIO.
instance MonadIO Ghc where
  liftIO = GhcMonad.liftIO

-- | A request.
data Request = Request Integer Cmd

-- | A response.
data Response = Response Integer ResultType
  deriving Show

-- | A command.
data Cmd
  = Ping Integer
  | Eval String
  | LoadFile FilePath
    deriving Show

-- | Custom decoding from s-expression.
instance L.FromLisp Request where
  parseLisp (L.List (L.Symbol "request":L.Number (I x):cmd:_)) = do
    cmd <- L.parseLisp cmd
    return (Request x cmd)
  parseLisp l = L.typeMismatch "Request" l

-- | Custom decoding from s-expression.
instance L.FromLisp Cmd where
  parseLisp (L.List (L.Symbol "ping":L.Number (I x):xs)) = return (Ping x)
  parseLisp (L.List (L.Symbol "eval":L.String x:_)) = return (Eval (T.unpack x))
  parseLisp (L.List (L.Symbol "load-file":L.String x:_)) = return (LoadFile (T.unpack x))
  parseLisp l = L.typeMismatch "Cmd" l

-- | A command result.
data Result
  = BadInput String
  | Unit
  | Pong Integer
  | EvalResult String
  | LoadResult SuccessFlag
  deriving Show

instance Show SuccessFlag where
  show Succeeded = "Succeeded"
  show Failed = "Failed"

instance L.ToLisp SuccessFlag where
  toLisp Succeeded = L.Symbol "succeeded"
  toLisp Failed = L.Symbol "failed"

-- | Custom encoding to s-expression.
instance L.ToLisp Result where
  toLisp (BadInput i) = L.List [L.Symbol "bad-input"
                               ,L.String (T.pack i)]
  toLisp Unit = L.List []
  toLisp (Pong i) = L.List [L.Symbol "pong",L.Number (I i)]
  toLisp (EvalResult x) = L.List [L.Symbol "eval-result"
                                 ,L.String (T.pack x)]
  toLisp (LoadResult r) = L.List [L.String "load-result"
                                 ,L.toLisp r]

-- | A GHC slave.
data Slave = Slave
  { slaveIn :: Chan (Ghc ())
  , slaveThread :: ThreadId
  }

-- | A general server.
data Server = Server
  { serverIn :: Chan (Cmd,Chan ResultType)
  , serverThread :: ThreadId
  , serverSlave :: Slave
  }

data ResultType
  = EndResult Result
  | Result Result
    deriving Show

-- | Custom encoding to s-expression.
instance L.ToLisp ResultType where
  toLisp (Result r) = L.List [L.Symbol "result"
                             ,L.toLisp r]
  toLisp (EndResult r) = L.List [L.Symbol "end-result"
                                ,L.toLisp r]

-- | Custom encoding to s-expression.
instance L.ToLisp Response where
  toLisp (Response id result) = L.List [L.Symbol "response"
                                       ,L.toLisp id
                                       ,L.toLisp result]
