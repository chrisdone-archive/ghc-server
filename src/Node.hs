module Node where

import Control.Monad
import System.Exit
import System.Process

-- | Read all stuff from a process.
node :: String -> IO ()
node input = do
  result <- readAllFromProcess "node" [] input
  case result of
    Left err -> error err
    Right (stderr,stdout) -> do
      when (not (null stderr)) (putStrLn stderr)
      when (not (null stdout)) (putStrLn stdout)

-- | Read all stuff from a process.
readAllFromProcess :: FilePath -> [String] -> String -> IO (Either String (String,String))
readAllFromProcess program flags input = do
  (code,out,err) <- readProcessWithExitCode program flags input
  return $ case code of
    ExitFailure _ -> Left err
    ExitSuccess   -> Right (err, out)
