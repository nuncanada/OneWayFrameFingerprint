{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_OneWayFrameFingerprint (
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

bindir     = "C:\\Users\\flavi\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\flavi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\OneWayFrameFingerprint-0.1.0.0-1sfQ6uJijjEiDA3tz2Zqi"
dynlibdir  = "C:\\Users\\flavi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\flavi\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\OneWayFrameFingerprint-0.1.0.0"
libexecdir = "C:\\Users\\flavi\\AppData\\Roaming\\cabal\\OneWayFrameFingerprint-0.1.0.0-1sfQ6uJijjEiDA3tz2Zqi\\x86_64-windows-ghc-8.4.3\\OneWayFrameFingerprint-0.1.0.0"
sysconfdir = "C:\\Users\\flavi\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "OneWayFrameFingerprint_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "OneWayFrameFingerprint_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "OneWayFrameFingerprint_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "OneWayFrameFingerprint_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "OneWayFrameFingerprint_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "OneWayFrameFingerprint_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)