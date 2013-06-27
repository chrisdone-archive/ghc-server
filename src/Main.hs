-- TODO: Next steps
--
-- 1. Figure out how to load modules.
-- 2. Figure out how to type check modules.
-- 3. Type of symbol/info of symbol.
-- 4. Figure out how to capture stdin.
--

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to the server.

module Main where

import           Server.Import
import           Server.Sexp
import           Server.Client

import           Control.Monad.Fix
import qualified Data.AttoLisp as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           GHC
import           GHC.Paths
import           Packages
import           System.IO

-- | Main entry point.
main :: IO ()
main =
  withSocketsDo startAccepter

-- | Start a server.
startAccepter :: IO ()
startAccepter =
  do server <- newServer
     socket <- listenOn (PortNumber port)
     logger (Notice ("Listening on port " ++ show port ++ " ..."))
     finally (forever (onException (do (handle,host,remotePort) <- accept socket
                                       forkIO (startClient handle host remotePort server))
                                   (logger (Fatal "Exception."))))
             (do logger (Fatal "Server killed.")
                 killServer server
                 sClose socket)

  where port = 5233

-- | Make a new server that will receive commands concurrently and
-- respond, asynchronously, concurrently.
newServer :: IO Server
newServer =
  do slave@Slave{..} <- newSlave
     inChan <- newChan
     let server = Server inChan
     tid <- forkIO (forever (do (cmd,chan) <- liftIO (readChan inChan)
                                forkIO (clientCall (writeChan slaveIn) cmd chan)))
     return (Server inChan tid slave)

killServer :: Server -> IO ()
killServer Server{serverSlave=Slave{..},..} =
  do killThread serverThread
     killThread slaveThread

-- | Start a new GHC slave.
newSlave :: IO Slave
newSlave =
 do inChan <- newChan
    tid <- forkIO (runGhc (Just libdir)
                          (do logger (Notice "Starting GHC ...")
                              initializeSlave
                              runSlave inChan))
    return (Slave inChan tid)

-- | Initialize the GHC service.
initializeSlave :: Ghc ()
initializeSlave =
  do initialDynFlags <- getSessionDynFlags
     setSessionDynFlags initialDynFlags
     (dflags',_,_)   <- parseDynamicFlags initialDynFlags (map (mkGeneralLocated "flag") flags)
     _pkgs           <- setSessionDynFlags dflags' { ghcLink = LinkInMemory
                                                   , hscTarget = HscInterpreted
                                                   }

     dflags          <- getSessionDynFlags
     (dflags,_pkgs) <- liftIO $ initPackages dflags
     setSessionDynFlags dflags
     mapM (fmap IIDecl . parseImportDecl) imports >>= setContext
     return ()

  where flags = ["-package ghc","-isrc"] :: [String]
        imports = ["import Prelude"]

-- | Run a GHC slave. This will receive commands and execute them
-- sequentially in a single thread.
runSlave slaveIn =
  do actions <- liftIO (getChanContents slaveIn)
     forM_ actions id

-- | Start a new client.
startClient :: Show a => Handle -> String -> a -> Server -> IO b
startClient handle host remotePort Server{..} =
  do logger (Notice ("Client connected from " ++ host ++ " on port " ++ show remotePort))
     hSetBuffering handle LineBuffering
     forever (do line <- B.hGetLine handle
                 case fromLispString line of
                   Left err ->
                     do let reply x = do hostLogger Debug ("<- " ++ show x)
                                         hPutLn handle l
                                           where l = L.encode x
                        hostLogger Error (show line)
                        hostLogger Error err
                        reply (BadInput err)
                   Right (Request id cmd) ->
                     do hostLogger Debug (show id ++ " -> " ++ show cmd)
                        let reply done x = do let direction = if done then " <- " else " .. "
                                              hostLogger Debug (show id ++ direction ++ show x)
                                              hPutLn handle l
                                                where l = L.encode (Response id x)
                        out <- newChan
                        writeChan serverIn (cmd,out)
                        fix (\loop ->
                              do response <- readChan out
                                 case response of
                                   Result r -> do reply False response
                                                  loop
                                   EndResult r -> reply True response))
  where hostLogger typ text = logger (typ (host ++ ":" ++ show remotePort ++ ": " ++ text))
        hPutLn :: Handle -> L.ByteString -> IO ()
        hPutLn h ps = L.hPut h ps >> L.hPut h (L.singleton 0x0a)
