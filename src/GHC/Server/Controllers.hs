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
  ,usesAt
  ,infoOf
  ,set
  ,packageConf
  ,setCurrentDir)
  where

import GHC.Server.Controller.Context
import GHC.Server.Controller.Debug
import GHC.Server.Controller.Eval
import GHC.Server.Controller.Info
import GHC.Server.Controller.Load
import GHC.Server.Controller.REPL
