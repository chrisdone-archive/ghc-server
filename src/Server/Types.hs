{-# LANGUAGE OverloadedStrings #-}

module Server.Types where

import           BasicTypes
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Trans
import qualified Data.AttoLisp as L
import           Data.Attoparsec.Number (Number(I))
import           Data.Data hiding (tyConName)
import           Data.Generics.Aliases
import qualified Data.Text as T
import           GHC
import           GhcMonad

-- | A log type.
data Log
  = Debug String
  | Notice String
  | Error String
  | Fatal String
  deriving (Show)

-- | A request.
data Request = Request Integer Cmd

-- | A response.
data Response = Response Integer ResultType
  deriving Show

-- | A command.
data Cmd
  = Ping Integer
  | Eval String
  | LoadTarget String
  | TypeOf String
  | KindOf String
  | InfoOf String
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
  parseLisp (L.List (L.Symbol "type":L.String x:_)) = return (TypeOf (T.unpack x))
  parseLisp (L.List (L.Symbol "kind":L.String x:_)) = return (KindOf (T.unpack x))
  parseLisp (L.List (L.Symbol "info":L.String x:_)) = return (InfoOf (T.unpack x))
  parseLisp (L.List (L.Symbol "load-target":L.String t:_)) = return (LoadTarget (T.unpack t))
  parseLisp l = L.typeMismatch "Cmd" l

-- | A command result.
data Result
  = BadInput String
  | Unit
  | Pong Integer
  | EvalResult String
  | LoadResult SuccessFlag
  | TypeResult String
  | KindResult String
  | InfoResult String
  deriving Show

-- | Custom encoding to s-expression.
instance L.ToLisp Result where
  toLisp (BadInput i) = L.List [L.Symbol "bad-input"
                               ,L.String (T.pack i)]
  toLisp Unit = L.List []
  toLisp (Pong i) = L.List [L.Symbol "pong",L.Number (I i)]
  toLisp (EvalResult x) = L.List [L.Symbol "eval-result",L.String (T.pack x)]
  toLisp (TypeResult x) = L.List [L.Symbol "type-result",L.String (T.pack x)]
  toLisp (KindResult x) = L.List [L.Symbol "kind-result",L.String (T.pack x)]
  toLisp (InfoResult x) = L.List [L.Symbol "info-result",L.String (T.pack x)]
  toLisp (LoadResult r) = L.List [L.String "load-result",L.toLisp r]

-- | A GHC slave.
data Slave = Slave
  { slaveIn :: Chan (SomeException -> IO (),Ghc ())
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
  | ErrorResult SomeException
    deriving Show

-- | Custom encoding to s-expression.
instance L.ToLisp ResultType where
  toLisp (Result r) = L.List [L.Symbol "result"
                             ,L.toLisp r]
  toLisp (EndResult r) = L.List [L.Symbol "end-result"
                                ,L.toLisp r]
  toLisp (ErrorResult r) = L.List [L.Symbol "error-result"
                                  ,L.toLisp r]

instance L.ToLisp SomeException where
  toLisp e = L.toLisp (show e)

-- | Custom encoding to s-expression.
instance L.ToLisp Response where
  toLisp (Response id result) = L.List [L.Symbol "response"
                                       ,L.toLisp id
                                       ,L.toLisp result]

--------------------------------------------------------------------------------
-- GHC data type instances

-- | GHC has its own MonadIO.
instance MonadIO Ghc where
  liftIO = GhcMonad.liftIO

instance Show SuccessFlag where
  show Succeeded = "Succeeded"
  show Failed = "Failed"

instance L.ToLisp SuccessFlag where
  toLisp Succeeded = L.Symbol "succeeded"
  toLisp Failed = L.Symbol "failed"

instance Show TargetId where
  show (TargetModule mn) = show mn

instance Show ModuleName where
  show x = "(ModuleName " ++ show (moduleNameString x) ++ ")"

instance L.FromLisp TargetId where
  parseLisp (L.String i) =
    if T.isSuffixOf ".hs" i
       then return (TargetFile (T.unpack i) Nothing)
       else return (TargetModule (mkModuleName (T.unpack i)))

--------------------------------------------------------------------------------
-- Misc

gshow :: Data a => a -> String
gshow x = gshows x ""

-- | A shows printer which is good for printing Core.
gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS) where
  render t
    | isTuple = showChar '('
              . drop 1
              . commaSlots
              . showChar ')'
    | isNull = showString "[]"
    | isList = showChar '['
             . drop 1
             . listSlots
             . showChar ']'
    | otherwise = showChar '('
                . constructor
                . slots
                . showChar ')'

    where constructor = showString . showConstr . toConstr $ t
          slots = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
          commaSlots = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
          listSlots = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t

          isTuple = all (==',') (filter (not . flip elem "()") (constructor ""))
          isNull = null (filter (not . flip elem "[]") (constructor ""))
          isList = constructor "" == "(:)"
