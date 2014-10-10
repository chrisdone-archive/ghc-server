{-# LANGUAGE CPP #-}

-- | Cabal integration.

module GHC.Server.Cabal where

import Data.Maybe
import Data.Monoid
import Data.Version
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Language.Haskell.Extension

-- | Get the name of the package being developed.
getPackageName :: IO String
getPackageName =
  do lconfig <- getPersistBuildConfig "dist"
     return (case pkgName (package (localPkgDescr lconfig))  of
               PackageName str -> str)

-- | Get package databases and resolved package dependencies.
getDependencyInfo :: IO ([FilePath],[String],[PackageId])
getDependencyInfo =
  do lconfig <- getPersistBuildConfig "dist"
     let libdeps = getDeps lconfig
         lpkgdsc = localPkgDescr lconfig
         libinfo = fmap (libBuildInfo) (library lpkgdsc)
         extensions = maybe [] (map showExt . defaultExtensions) libinfo
         libsourceDirs = fmap (hsSourceDirs) libinfo
     return (fromMaybe [] libsourceDirs,
             extensions,
             map snd (fromMaybe [] libdeps))

-- | Convert an extension to a paramter.
showExt :: Extension -> String
showExt g =
  case g of
    EnableExtension e -> "-X" <> show e
    DisableExtension e -> "-XNo" <> show e
    UnknownExtension e -> "-X" <> show e

-- | Render package id to foo-1.2.3
renderPackageId :: PackageId -> String
renderPackageId pid = unPkgName (pkgName pid) <> "-" <> showVersion (pkgVersion pid)
  where unPkgName (PackageName n) = n

-- | Get dependencies. FIXME: Deal with targets.
getDeps :: LocalBuildInfo -> Maybe [(InstalledPackageId, PackageId)]
#if MIN_VERSION_Cabal(1,18,0)
getDeps = fmap (componentPackageDeps . (\(_,x,_) -> x)) . listToMaybe . componentsConfigs
#else
getDeps = fmap componentPackageDeps . libraryConfig
#endif
