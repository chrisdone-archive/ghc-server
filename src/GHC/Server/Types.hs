{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Server.Types where

import           Control.Concurrent
import           Control.Exception
import qualified Data.AttoLisp as L
import           Data.ByteString (ByteString)
import           Data.Data
import           Data.Generics.Aliases
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Compat hiding (MonadIO)

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
  | Set String
  | PackageConf FilePath
    deriving Show

-- | Custom decoding from s-expression.
instance L.FromLisp Request where
  parseLisp (L.List (L.Symbol "request":i:cmd:_)) = do
    cmd <- L.parseLisp cmd
    x <- L.parseLisp i
    return (Request x cmd)
  parseLisp l = L.typeMismatch "Request" l

-- | Custom decoding from s-expression.
instance L.FromLisp Cmd where
  parseLisp (L.List (L.Symbol "ping":i:xs)) =
    do x <- L.parseLisp i
       return (Ping x)
  parseLisp (L.List (L.Symbol "eval":L.String x:_)) = return (Eval (T.unpack x))
  parseLisp (L.List (L.Symbol "type":L.String x:_)) = return (TypeOf (T.unpack x))
  parseLisp (L.List (L.Symbol "kind":L.String x:_)) = return (KindOf (T.unpack x))
  parseLisp (L.List (L.Symbol "info":L.String x:_)) = return (InfoOf (T.unpack x))
  parseLisp (L.List (L.Symbol "load-target":L.String t:_)) = return (LoadTarget (T.unpack t))
  parseLisp (L.List (L.Symbol "set":L.String opt:_)) = return (Set (T.unpack opt))
  parseLisp (L.List (L.Symbol "package-conf":L.String pkgconf:_)) = return (PackageConf (T.unpack pkgconf))
  parseLisp l = L.typeMismatch "Cmd" l

-- | A command result.
data Result
  = BadInput String
  | Unit
  | Pong Integer
  | EvalResult String
  | EvalImport [String]
  | EvalStdout ByteString
  | EvalStderr ByteString
  | LoadResult SuccessFlag
  | TypeResult String
  | KindResult String
  | DeclResult [String]
  | InfoResult String
  | LogResult Severity SrcSpan String
  deriving Show

-- | Custom encoding to s-expression.
instance L.ToLisp Result where
  toLisp Unit =
    L.List []
  toLisp (EvalStdout e) =
    L.List [L.Symbol "eval-stdout",L.toLisp (T.decodeUtf8 e)]
  toLisp (EvalImport e) =
    L.List [L.Symbol "eval-import",L.toLisp e]
  toLisp (EvalStderr e) =
    L.List [L.Symbol "eval-stderr",L.toLisp (T.decodeUtf8 e)]
  toLisp (BadInput i) =
    L.List [L.Symbol "bad-input",L.toLisp i]
  toLisp (Pong i) =
    L.List [L.Symbol "pong",L.toLisp i]
  toLisp (EvalResult x) =
    L.List [L.Symbol "eval-result",L.toLisp x]
  toLisp (TypeResult x) =
    L.List [L.Symbol "type-result",L.toLisp x]
  toLisp (DeclResult x) =
    L.List [L.Symbol "decl-result",L.toLisp x]
  toLisp (KindResult x) =
    L.List [L.Symbol "kind-result",L.toLisp x]
  toLisp (InfoResult x) =
    L.List [L.Symbol "info-result",L.toLisp x]
  toLisp (LoadResult r) =
    L.List [L.Symbol "load-result",L.toLisp r]
  toLisp (LogResult severity span msg) =
    L.List [L.Symbol "log-result"
           ,L.toLisp severity
           ,L.toLisp span
           ,L.toLisp msg]

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

deriving instance Show Severity

instance L.ToLisp Severity where
  toLisp t =
    L.Symbol (case t of
                SevOutput -> "output"
                SevInfo -> "info"
                SevError -> "error"
                SevWarning -> "warning"
                SevFatal -> "fatal")

instance L.ToLisp SrcSpan where
  toLisp (RealSrcSpan realsrcspan) = L.toLisp realsrcspan
  toLisp (UnhelpfulSpan fs) = L.toLisp fs

instance L.ToLisp RealSrcSpan where
  toLisp span =
    L.List [L.toLisp (srcSpanFile span)
           ,L.toLisp (srcSpanStartLine span)
           ,L.toLisp (srcSpanEndLine span)
           ,L.toLisp (srcSpanStartCol span)
           ,L.toLisp (srcSpanEndCol span)]

instance L.ToLisp FastString where
  toLisp = L.toLisp . unpackFS

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
