module JSBeautify
  (beautify
  ,beautifyJS)
  where

import Language.ECMAScript3.Syntax as ECMA
import Language.ECMAScript3.PrettyPrint as ECMA
import System.Exit
import System.Process
import Text.PrettyPrint as Doc

beautifyJS = beautify . Doc.render . ECMA.javaScript . ECMA.Script ()

beautify :: String -> IO String
beautify src = do
  result <- readAllFromProcess "/home/chris/Projects/js-beautify/python/js-beautify" ["-i","-s","2","-d","-w","80"] src
  case result of
    Left err -> error err
    Right (_,out) -> return out

-- | Read all stuff from a process.
readAllFromProcess :: FilePath -> [String] -> String -> IO (Either String (String,String))
readAllFromProcess program flags input = do
  (code,out,err) <- readProcessWithExitCode program flags input
  return $ case code of
    ExitFailure _ -> Left err
    ExitSuccess   -> Right (err, out)
