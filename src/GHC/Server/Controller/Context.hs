-- | Context-configuring controller. Directory, packages, options, etc.

module GHC.Server.Controller.Context (set,packageConf,setCurrentDir) where

import           GHC.Compat
import           GHC.Server.Duplex
import           GHC.Server.Types

import           Control.Monad
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory

-- | Set the options.
set :: Text -> Unit
set flag =
  withGhc (setFlag (T.unpack flag))

-- | Set the package conf.
packageConf :: FilePath -> Unit
packageConf pkgconf =
  withGhc (do setFlag ("-package-conf=" <> pkgconf)
              df <- getSessionDynFlags
              (dflags,_pkgs) <- io (initPackages df)
              _ <- setSessionDynFlags dflags
              return ())

-- | Set the current directory.
setCurrentDir :: FilePath -> Unit
setCurrentDir dir =
  withGhc (do io (setCurrentDirectory dir)
              workingDirectoryChanged
              setTargets []
              _ <- load LoadAllTargets
              return ())

-- | Apply a flag.
setFlag :: GhcMonad m => String -> m ()
setFlag flag =
  do df <- getSessionDynFlags
     (dflags,_,_) <- parseDynamicFlags
                       df
                       (map (mkGeneralLocated "flag")
                            [flag])
     void (setSessionDynFlags dflags)
