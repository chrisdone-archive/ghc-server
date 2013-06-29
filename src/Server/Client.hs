{-# LANGUAGE RecordWildCards #-}

-- | Client connection handler and message forwarder/receiver.

module Server.Client where

import           Server.Import
import           Server.Sexp

import           Control.Monad.Fix
import qualified Data.AttoLisp as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           System.IO

-- | Start a new client. It waits for lines of s-expression commands,
-- parses them into proper requests, if the request is proper, it will
-- handle it in "handleRequest".
startClient :: Show a => Handle -> String -> a -> Server -> IO b
startClient handle host remotePort server@Server{..} =
  do logger (Notice ("Client connected from " ++ host ++ " on port " ++ show remotePort))
     hSetBuffering handle LineBuffering
     forever (do line <- B.hGetLine handle
                 case fromLispString line of
                   Left err ->
                     do hostLogger Error (show line)
                        hostLogger Error err
                        let reply = BadInput err
                        hostLogger Debug ("<- " ++ show reply)
                        hPutLn handle (L.encode reply)
                   Right (Request id cmd) ->
                     void (forkIO (handleRequest handle id cmd server hostLogger)))
  where hostLogger typ text = logger (typ (host ++ ":" ++ show remotePort ++ ": " ++ text))

-- | Handle an incoming command request from the client. It will send
-- a command to the server, passing it a channel, and then read all
-- responses from that channel and forward them back over the
-- connection.
handleRequest :: Handle -> Integer -> Cmd -> Server
              -> ((String -> Log) -> [Char] -> IO a) -> IO ()
handleRequest handle id cmd Server{..} hostLogger =
  do hostLogger Debug (show id ++ " -> " ++ show cmd)
     out <- newChan
     writeChan serverIn (cmd,out)
     fix (\loop ->
           do response <- readChan out
              case response of
                Result r -> do reply False response
                               loop
                EndResult r -> reply True response
                ErrorResult e -> reply True response)

  where reply done x = do let direction = if done then " <- " else " .. "
                          hostLogger Debug (show id ++ direction ++ show x)
                          hPutLn handle (L.encode (Response id x))

-- | A way to put a line, for some reason this doesn't exist in the library.
hPutLn :: Handle -> L.ByteString -> IO ()
hPutLn h ps = L.hPut h ps >> L.hPut h (L.singleton 0x0a)
