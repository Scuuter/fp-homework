{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw3 (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.3/8.6.3/bin"
libdir     = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.3/8.6.3/lib/x86_64-linux-ghc-8.6.3/hw3-0.1.0.0-7qLJWLvJJvHH0pgMb32erC-hw3"
dynlibdir  = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.3/8.6.3/lib/x86_64-linux-ghc-8.6.3"
datadir    = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.3/8.6.3/share/x86_64-linux-ghc-8.6.3/hw3-0.1.0.0"
libexecdir = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.3/8.6.3/libexec/x86_64-linux-ghc-8.6.3/hw3-0.1.0.0"
sysconfdir = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.3/8.6.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
