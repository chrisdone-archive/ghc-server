{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Server interface to GHC.

module GHC.Server
  (startServer)
  where

import           GHC.Compat
import           GHC.Server.Commands
import           GHC.Server.Ghc
import           GHC.Server.Logging
import           GHC.Server.TH
import           GHC.Server.Types

import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Concurrent.STM
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.AttoLisp (FromLisp(..),Lisp,lisp)
import qualified Data.AttoLisp as L
import qualified Data.Attoparsec as P
import           Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import qualified Data.Text as T
import           Network
import           System.IO

--------------------------------------------------------------------------------
-- Serving

-- | Start the server.
startServer :: Int -> LoggingT IO ()
startServer port =
  do socket <- io (listenOn (PortNumber (fromIntegral port)))
     ghcChan <- io startGhc
     modInfos <- io (atomically (newTVar mempty))
     let state = State modInfos
     $(logInfo)
       ("Listening on port " <>
        T.pack (show port) <>
        " ...")
     forever (do (h,host,_port) <- io (accept socket)
                 io (startClient state ghcChan host h))

-- | Run a command handling client.
startClient :: State -> Chan (Ghc ()) -> String -> Handle -> IO ()
startClient state ghcChan host h =
  void (forkIO (runLogging go))
  where go =
          do $(logInfo) ("New connection from " <> T.pack host)
             activeCommands <- io (atomically (newTVar mempty))
             io (hSetBuffering h LineBuffering)
             fix (\loop ->
                    do mline <- (io (E.catch (fmap Just (SB.hGetLine h))
                                             (\(_ :: IOException) ->
                                                return Nothing)))
                       case mline of
                         Just line ->
                           do handleLispLine state ghcChan h activeCommands line
                              loop
                         _ -> return ())
             $(logInfo) ("Connection closed to " <> T.pack host)

--------------------------------------------------------------------------------
-- GHC slave

-- | Start the GHC slave.
startGhc :: IO (Chan (Ghc ()))
startGhc =
  do chan <- newChan
     void (forkIO (runGhc (Just libdir)
                          (do runLogging initializeGhc
                              forever (protect (join (io (readChan chan)))))))
     return chan
  where protect m =
          gcatch m
                 (\(SomeException e) ->
                    runLogging
                      ($(logInfo)
                         ("GHC exception: " <>
                          T.pack (show e))))

--------------------------------------------------------------------------------
-- Command handling

-- | Handle an incoming Lisp-encoded line.
handleLispLine :: State
               -> Chan (Ghc ())
               -> Handle
               -> TVar (HashMap Integer SomeChan)
               -> ByteString
               -> LoggingT IO ()
handleLispLine state ghcChan h activeCommands line =
  case fromLispString line of
    Left e ->
      $(logError) ("Erroneous input Lisp: " <> T.pack e)
    Right (Incoming ix input :: Incoming Lisp) ->
      case input of
        Request (SomeCommand c) ->
          do $(logDebug)
               ("Some command: " <>
                T.pack (show c))
             inputChan <- io (newInputChan c)
             io (atomically
                   (modifyTVar activeCommands
                               (M.insert ix (SomeChan inputChan))))
             void (io (forkIO (do runLogging ($(dispatch ''Command) ghcChan ix h inputChan c state)
                                  atomically
                                    (modifyTVar activeCommands
                                                (M.delete ix)))))
        FeedIn i ->
          do $(logDebug)
               ("Command " <>
                T.pack (show ix) <>
                " feed: " <>
                T.pack (show i))
             cmds <- io (atomically (readTVar activeCommands))
             case M.lookup ix cmds of
               Nothing ->
                 $(logError)
                   ("No active command for " <>
                    T.pack (show ix))
               Just (SomeChan chan) ->
                 do $(logError)
                      ("Feeding " <>
                       T.pack (show ix))
                    case L.parseEither parseLisp i of
                      Right i' ->
                        io (writeChan chan i')
                      Left err ->
                        $(logError)
                          ("Couldn't parse input feed into proper type: " <>
                           T.pack (show err))
  where newInputChan :: Command (Duplex i o r) -> IO (Chan i)
        newInputChan _ = newChan

--------------------------------------------------------------------------------
-- Lisp parsing

-- | Parse a single s-expr followed by optional whitespace and end of
--   file.
parseLispOnly :: ByteString -> Either String Lisp
parseLispOnly b =
  case P.parseOnly lisp b of
    Left err ->
      Left ("Bad s-expression: " <> err)
    Right ok -> Right ok

-- | Parse a single s-expr.
fromLispString :: FromLisp a
               => SB.ByteString -> Either String a
fromLispString = parseLispOnly >=> L.parseEither parseLisp
