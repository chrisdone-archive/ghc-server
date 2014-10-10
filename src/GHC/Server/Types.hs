{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | All server types.

module GHC.Server.Types where

import           Control.Monad.Logger
import           GHC.Compat

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.AttoLisp (FromLisp(..),ToLisp(..))
import qualified Data.AttoLisp as L
import           Data.Attoparsec.Number
import           Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Duplex types

-- | State for the duplex.
data DuplexState i o =
  DuplexState {stateIn :: !(Chan i)
              ,stateOut :: !(Chan o)
              ,stateGhc :: !(Chan (Ghc ()))}

-- | Duplexing full duplex command handling monad.
newtype DuplexT m i o r =
  DuplexT {runDuplexT :: ReaderT (DuplexState i o) m r}
  deriving (Functor,Applicative,Monad,MonadIO)

instance ExceptionMonad (DuplexT Ghc i o) where
  gcatch (DuplexT (ReaderT fm)) fh =
    DuplexT (ReaderT (\r ->
                        gcatch (fm r)
                               (\e ->
                                  let DuplexT (ReaderT fh') = fh e
                                  in fh' r)))
  gmask getsF =
    DuplexT (ReaderT (\r ->
                        gmask (\f ->
                                 case getsF (\(DuplexT (ReaderT x')) ->
                                               DuplexT (ReaderT (f . x'))) of
                                   DuplexT (ReaderT rf) -> rf r)))

instance MonadLogger (DuplexT Ghc i o) where
  monadLoggerLog loc source level msg =
    liftIO (runStdoutLoggingT (monadLoggerLog loc source level msg))

instance HasDynFlags (DuplexT Ghc i o) where
  getDynFlags =
    DuplexT (ReaderT (const getDynFlags))

instance GhcMonad (DuplexT Ghc i o) where
  getSession =
    DuplexT (ReaderT (const getSession))
  setSession s =
    DuplexT (ReaderT (const (setSession s)))

instance ExceptionMonad (LoggingT Ghc) where
  gcatch (LoggingT fm) fh =
    LoggingT (\r ->
               gcatch (fm r)
                      (\e ->
                         let (LoggingT fh') = fh e
                         in fh' r))
  gmask getsF =
    LoggingT (\r ->
               gmask (\f ->
                        case getsF (\(LoggingT x') ->
                                      LoggingT (f . x')) of
                          (LoggingT rf) -> rf r))

instance HasDynFlags (LoggingT Ghc) where
  getDynFlags =
    LoggingT (const getDynFlags)

instance GhcMonad (LoggingT Ghc) where
  getSession =
    LoggingT (const getSession)
  setSession s =
    LoggingT (const (setSession s))

type Duplex i o r = DuplexT IO i o r

-- | Command that only produces duplexing results.
type Producer o r = Duplex () o r

-- | Command that only returns a result.
type Returns r = Duplex () () r

-- | Command that returns no results at all.
type Unit = Duplex () () ()

--------------------------------------------------------------------------------
-- Transport layer types

-- | A input payload wrapper.
data Incoming i =
  Incoming !Integer
           !(Input i)
  deriving (Show)

-- | An output input payload wrapper.
data Outgoing o =
  Outgoing !Integer
           !(Output o)
  deriving (Show)

-- | An input value for some serialization type.
data Input i
  = Request !SomeCommand
  | FeedIn !i
  deriving (Show)

-- | An input value for some serialization type.
data Output o
  = EndResult !o
  | FeedOut !o
  | ErrorResult !SomeException
  deriving (Show)

-- | Outputable things.
type Outputish a = (ToLisp a,Show a)

-- | Inputable things.
type Inputish a = (FromLisp a,Show a)

-- | Generic command.
data SomeCommand =
  forall i o r. (Inputish i,Outputish o,Outputish r) => SomeCommand (Command (Duplex i o r))

-- | A generic channel.
data SomeChan = forall a. Inputish a => SomeChan (Chan a)

--------------------------------------------------------------------------------
-- Transport serialization code

instance FromLisp l => FromLisp (Incoming l) where
  parseLisp (L.List (L.Symbol "request" :i:input:_)) =
    do input' <- parseLisp input
       x <- parseLisp i
       return (Incoming x (Request input'))
  parseLisp (L.List (L.Symbol "feed" :i:input:_)) =
    do input' <- parseLisp input
       x <- parseLisp i
       return (Incoming x (FeedIn input'))
  parseLisp l = L.typeMismatch "Incoming" l

instance ToLisp l => ToLisp (Outgoing l) where
  toLisp (Outgoing ix output) =
    case output of
      EndResult o ->
        L.List [L.Symbol "end-result",toLisp ix,toLisp o]
      FeedOut o ->
        L.List [L.Symbol "result",toLisp ix,toLisp o]
      ErrorResult o ->
        L.List [L.Symbol "error-result",toLisp ix,toLisp o]

deriving instance Show SomeCommand
instance L.FromLisp SomeCommand where
  parseLisp (L.List (L.Symbol "ping":i:_)) =
    do x <- L.parseLisp i
       return (SomeCommand (Ping x))
  parseLisp (L.List (L.Symbol "eval":L.String x:_)) =
    return (SomeCommand (Eval x))
  parseLisp (L.List (L.Symbol "type-of":L.String x:_)) =
    return (SomeCommand (TypeOf x))
  parseLisp (L.List (L.Symbol "type-at":L.String fp:L.String string:L.Number (I sl):L.Number (I sc):L.Number (I el):L.Number (I ec):_)) =
    return (SomeCommand
              (TypeAt (T.unpack fp)
                      string
                      (fromIntegral sl)
                      (fromIntegral sc)
                      (fromIntegral el)
                      (fromIntegral ec)))
  parseLisp (L.List (L.Symbol "uses":L.String fp:L.String string:L.Number (I sl):L.Number (I sc):L.Number (I el):L.Number (I ec):_)) =
    return (SomeCommand
              (Uses (T.unpack fp)
                    string
                    (fromIntegral sl)
                    (fromIntegral sc)
                    (fromIntegral el)
                    (fromIntegral ec)))
  parseLisp (L.List (L.Symbol "loc-at":L.String fp:L.String string:L.Number (I sl):L.Number (I sc):L.Number (I el):L.Number (I ec):_)) =
    return (SomeCommand
              (LocationAt (T.unpack fp)
                          string
                          (fromIntegral sl)
                          (fromIntegral sc)
                          (fromIntegral el)
                          (fromIntegral ec)))
  parseLisp (L.List (L.Symbol "kind-of":L.String x:_)) =
    return (SomeCommand (KindOf x))
  parseLisp (L.List (L.Symbol "info":L.String x:_)) =
    return (SomeCommand (InfoOf x))
  parseLisp (L.List (L.Symbol "load-target":L.String t:_)) =
    return (SomeCommand (LoadTarget t))
  parseLisp (L.List (L.Symbol "set":L.String opt:_)) =
    return (SomeCommand (Set opt))
  parseLisp (L.List (L.Symbol "package-conf":L.String pkgconf:_)) =
    return (SomeCommand (PackageConf (T.unpack pkgconf)))
  parseLisp (L.List (L.Symbol "cd":L.String dir:_)) =
    return (SomeCommand (SetCurrentDir (T.unpack dir)))
  parseLisp l = L.typeMismatch "Cmd" l

instance ToLisp SomeException where
  toLisp e = toLisp (show e)

--------------------------------------------------------------------------------
-- Commands

-- | Command.
data Command a where
  LoadTarget    :: Text -> Command (Producer Msg (SuccessFlag,Integer))
  Eval          :: Text -> Command (Duplex Text Text EvalResult)
  Ping          :: Integer -> Command (Returns Integer)
  TypeOf        :: Text -> Command (Returns Text)
  LocationAt    :: FilePath -> Text -> Int -> Int -> Int -> Int -> Command (Returns SrcSpan)
  TypeAt        :: FilePath -> Text -> Int -> Int -> Int -> Int -> Command (Returns Text)
  Uses          :: FilePath -> Text -> Int -> Int -> Int -> Int -> Command (Returns Text)
  KindOf        :: Text -> Command (Returns Text)
  InfoOf        :: Text -> Command (Returns [Text])
  Set           :: Text -> Command (Returns ())
  PackageConf   :: FilePath -> Command (Returns ())
  SetCurrentDir :: FilePath -> Command (Returns ())

deriving instance Show (Command a)

-- | Evaluation result.
data EvalResult =
  NewContext [String]
  deriving (Show)

instance ToLisp EvalResult where
  toLisp (NewContext is) =
    L.List [L.Symbol "new-context",toLisp is]

-- | A message.
data Msg =
  Msg !Severity
      !SrcSpan
      !Text
  deriving (Show)

deriving instance Show Severity

instance ToLisp Msg where
  toLisp (Msg sev span' text') =
    L.List [L.Symbol "msg",toLisp sev,toLisp span',toLisp text']

instance ToLisp Severity where
  toLisp t = L.Symbol (T.pack (showSeverity t))

instance Show SuccessFlag where
  show Succeeded = "Succeeded"
  show Failed = "Failed"

instance ToLisp SuccessFlag where
  toLisp Succeeded = L.Symbol "succeeded"
  toLisp Failed = L.Symbol "failed"

--------------------------------------------------------------------------------
-- Spans

instance ToLisp SrcSpan where
  toLisp (RealSrcSpan realsrcspan) = toLisp realsrcspan
  toLisp (UnhelpfulSpan fs) = toLisp fs

instance ToLisp RealSrcSpan where
  toLisp span' =
    L.List [toLisp (srcSpanFile span')
           ,toLisp (srcSpanStartLine span')
           ,toLisp (srcSpanEndLine span')
           ,toLisp (srcSpanStartCol span')
           ,toLisp (srcSpanEndCol span')]

instance ToLisp FastString where
  toLisp = toLisp . unpackFS
