{-# LANGUAGE OverloadedStrings #-}

-- | All server commands.

module GHC.Server.Commands where

import           GHC.Compat
import           GHC.Server.Duplex
import           GHC.Server.Eval
import           GHC.Server.Ghc
import           GHC.Server.Types

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Commands

-- | Load a module.
loadTarget :: Text -> Producer Msg (SuccessFlag,Integer)
loadTarget filepath =
  withGhc (do df <- getSessionDynFlags
              warnings <- liftIO (atomically (newTVar 0))
              result <- withMessages (recordMessage warnings df)
                                     doLoad
              count <- liftIO (atomically (readTVar warnings))
              return (result,count))
  where recordMessage warnings df =
          \sev sp doc ->
            do send (Msg sev sp (T.pack (showSDoc df doc)))
               case sev of
                 SevWarning ->
                   liftIO (atomically (modifyTVar' warnings (+ 1)))
                 _ -> return ()
        doLoad =
          do target <- guessTarget (T.unpack filepath)
                                   Nothing
             setTargets [target]
             result <- load LoadAllTargets
             loaded <- getModuleGraph >>= filterM isLoaded . map ms_mod_name
             mapM parseImportDecl
                  (["import Prelude"] <>
                   loadedImports loaded) >>=
               setContext
             return result

-- | Ping/pong.
ping :: Integer -> Returns Integer
ping = return

-- | Eval something for the REPL.
eval :: Text -> Duplex Text Text EvalResult
eval e = withGhc (tryImportOrDecls e)

-- | Type of identifier.
typeOf :: Text -> Returns Text
typeOf = undefined

-- | Location of identifier at point.
locationAt :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns SrcSpan
locationAt = undefined

-- | Type of identifier at point.
typeAt :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns Text
typeAt = undefined

-- | Find uses.
uses :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns Text
uses = undefined

-- | Kind of the identifier.
kindOf :: Text -> Returns Text
kindOf = undefined

-- | Info of the identifier.
infoOf :: Text -> Returns Text
infoOf = undefined

-- | Set the options.
set :: Text -> Unit
set = undefined

-- | Set the package conf.
packageConf :: FilePath -> Unit
packageConf = undefined

-- | Set the current directory.
setCurrentDir :: Text -> Unit
setCurrentDir = undefined
