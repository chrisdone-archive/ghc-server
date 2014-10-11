{-# LANGUAGE OverloadedStrings #-}

-- | All server commands. Some commands are re-exported from below
-- this hierarchy.

module GHC.Server.Commands
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
import GHC.Server.Commands.Debug
import GHC.Server.Commands.Context
import GHC.Server.Commands.Eval
import GHC.Server.Commands.Load
import GHC.Server.Commands.REPL
import GHC.Server.Types

import Data.Text (Text)

--------------------------------------------------------------------------------
-- Commands

-- | Location of identifier at point.
locationAt :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns SrcSpan
locationAt = undefined

-- | Type of identifier at point.
typeAt :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns Text
typeAt = undefined

-- | Find uses.
uses :: FilePath -> Text -> Int -> Int -> Int -> Int -> Returns Text
uses = undefined
