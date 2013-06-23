{-# LANGUAGE OverloadedStrings #-}

module Server.Types where

import           Control.Concurrent
import           Control.Monad.Trans
import qualified Data.AttoLisp as L
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

-- | A command.
data Cmd
  = Ping
  | Eval String

-- | Custom decoding from s-expression.
instance L.FromLisp Cmd where
  parseLisp (L.List (L.Symbol "ping":xs)) = return Ping
  parseLisp (L.List (L.Symbol "eval":L.String x:_)) = return (Eval (T.unpack x))
  parseLisp l = L.typeMismatch "Cmd" l

-- | A command result.
data Result = BadInput String | Unit | Pong | EvalResult String

-- | Custom encoding to s-expression.
instance L.ToLisp Result where
  toLisp (BadInput i) = L.List [L.Symbol "bad-input"
                               ,L.String (T.pack i)]
  toLisp Unit = L.List []
  toLisp Pong = L.List [L.Symbol "pong"]
  toLisp (EvalResult x) = L.List [L.Symbol "eval-result"
                                 ,L.String (T.pack x)]

data Slave = Slave
  { slaveIn :: MVar Cmd
  , slaveOut :: MVar Result
  }
