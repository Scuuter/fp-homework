{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw5 (
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

bindir     = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/bin"
libdir     = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/lib/x86_64-linux-ghc-8.6.4/hw5-0.1.0.0-6pmG6tCjJ8DCtDqMBH1Wxm-hw5-test"
dynlibdir  = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/share/x86_64-linux-ghc-8.6.4/hw5-0.1.0.0"
libexecdir = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/libexec/x86_64-linux-ghc-8.6.4/hw5-0.1.0.0"
sysconfdir = "/home/scuuter/Desktop/Functional/fp-homework/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw5_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw5_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw5_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw5_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw5_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw5_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
