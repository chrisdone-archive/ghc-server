{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to the server.

module Main where

import           Server.Import
import           Server.Sexp
import           Server.Client

import qualified Data.AttoLisp as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           GHC
import           GHC.Paths
import           System.IO

-- | Main entry point.
main :: IO ()
main =
  withSocketsDo startAccepter

-- | Start a server.
startAccepter :: IO ()
startAccepter =
  do slave <- newSlave
     socket <- listenOn (PortNumber port)
     logger (Notice ("Listening on port " ++ show port ++ " ..."))
     finally (forever (do (handle,host,remotePort) <- accept socket
                          forkIO (startClient handle host remotePort slave)))
             (sClose socket)

  where port = 5233

-- | Start a new GHC slave.
newSlave :: IO Slave
newSlave = do
  inVar <- newEmptyMVar
  outVar <- newEmptyMVar
  let slave = Slave inVar outVar
  forkIO (runGhc (Just libdir)
                 (do logger (Notice "Starting GHC ...")
                     initialize
                     runSlave slave))
  return slave

runSlave :: Slave -> Ghc ()
runSlave Slave{..} =
  forever (liftIO (takeMVar slaveIn) >>= clientCall >>= liftIO . putMVar slaveOut)

-- | Initialize the GHC service.
initialize :: Ghc ()
initialize =
  do initialDynFlags <- getProgramDynFlags
     (dflags',_,_)   <- parseDynamicFlags initialDynFlags (map (mkGeneralLocated "flag") flags)
     _pkgs           <- setSessionDynFlags dflags'
     dflags          <- getSessionDynFlags
     mapM (fmap IIDecl . parseImportDecl) imports >>= setContext
     return ()

  where flags = [] :: [String]
        imports = ["import Prelude"]

-- | Start a new client.
startClient :: Show a => Handle -> String -> a -> Slave -> IO b
startClient handle host remotePort Slave{..} =
  do logger (Notice ("Client connected from " ++ host ++ " on port " ++ show remotePort))
     hSetBuffering handle NoBuffering
     forever $ do
       line <- B.hGetLine handle
       logger (Debug (host ++ " -> " ++ show line))
       case fromLispString line of
         Left err -> do logger (Error err)
                        reply (BadInput err)
         Right cmd -> do putMVar slaveIn cmd
                         takeMVar slaveOut >>= reply

  where reply x = do logger (Debug (host ++ " <- " ++ show l))
                     hPutLn handle l
                       where l = L.encode x

hPutLn :: Handle -> L.ByteString -> IO ()
hPutLn h ps = L.hPut h ps >> L.hPut h (L.singleton 0x0a)
