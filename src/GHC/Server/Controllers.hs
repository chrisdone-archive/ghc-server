{-# LANGUAGE OverloadedStrings #-}

-- | All server commands. Some commands are re-exported from below
-- this hierarchy.

module GHC.Server.Controllers
  (loadTarget
  ,ping
  ,eval
  ,typeOf
  ,kindOf
  ,locationAt
  ,typeAt
  ,uses
  ,infoOf
  ,set
  ,packageConf
  ,setCurrentDir)
  where

import GHC.Compat
import GHC.Server.Controller.Debug
import GHC.Server.Controller.Context
import GHC.Server.Controller.Eval
import GHC.Server.Controller.Load
import GHC.Server.Controller.REPL
import GHC.Server.Types

import Data.Text (Text)

--------------------------------------------------------------------------------
-- Controller

-- | Location of identifier at point.
locationAt :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns SrcSpan
locationAt = undefined

-- | Type of identifier at point.
typeAt :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns Text
typeAt = undefined

-- | Find uses.
uses :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns Text
uses = undefined
