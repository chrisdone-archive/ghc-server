{-# OPTIONS -Wall #-}

-- | Main socket server.

module Server.Server where

import Server.Client
import Server.Commands
import Server.Import
import Server.Slave

-- | Start a server.
startAccepter :: IO ()
startAccepter =
  do main <- myThreadId
     server <- newServer main
     socket <- listenOn (PortNumber port)
     logger (Notice ("Listening on port " ++ show port ++ " ..."))
     finally (forever (do (handle,host,remotePort) <- accept socket
                          forkIO (startClient handle host remotePort server)))
             (do logger (Fatal "Server killed.")
                 sClose socket)

  where port = 5233

-- | Make a new server that will receive commands concurrently and
-- respond, asynchronously, concurrently.
newServer :: ThreadId -> IO Server
newServer main =
  do slave <- newSlave main
     inChan <- newChan
     let server = Server inChan
     -- Launch a command-accepting loop
     tid <- forkIO (forever (do (cmd,replyChan) <- liftIO (readChan inChan)
                                newRequest slave cmd replyChan))
     return (Server inChan tid slave)

-- | Launch a command handler, may be queued up in GHC or run
-- immediately.
newRequest :: Slave -> Cmd -> Chan ResultType -> IO ThreadId
newRequest slave cmd resultsChan =
  forkIO (do me <- myThreadId
             let onError e = writeChan resultsChan (ErrorResult e)
                 withGHC m = writeChan (slaveIn slave) (onError,m)
             clientCall withGHC cmd resultsChan)
