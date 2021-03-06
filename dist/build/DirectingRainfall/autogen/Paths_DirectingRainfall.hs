{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_DirectingRainfall (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/pascalengel/.cabal/bin"
libdir     = "/Users/pascalengel/.cabal/lib/x86_64-osx-ghc-8.8.1/DirectingRainfall-1.0.0.0-8cfpGQAMisfKqGljiz05J2-DirectingRainfall"
dynlibdir  = "/Users/pascalengel/.cabal/lib/x86_64-osx-ghc-8.8.1"
datadir    = "/Users/pascalengel/.cabal/share/x86_64-osx-ghc-8.8.1/DirectingRainfall-1.0.0.0"
libexecdir = "/Users/pascalengel/.cabal/libexec/x86_64-osx-ghc-8.8.1/DirectingRainfall-1.0.0.0"
sysconfdir = "/Users/pascalengel/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DirectingRainfall_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DirectingRainfall_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "DirectingRainfall_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "DirectingRainfall_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DirectingRainfall_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DirectingRainfall_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
