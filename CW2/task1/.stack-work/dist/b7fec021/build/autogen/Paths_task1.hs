{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_task1 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Users\\Daria\\Code2\\FP-2016\\CW2\\task1\\.stack-work\\install\\4338eac8\\bin"
libdir     = "D:\\Users\\Daria\\Code2\\FP-2016\\CW2\\task1\\.stack-work\\install\\4338eac8\\lib\\x86_64-windows-ghc-8.0.1\\task1-0.1.0.0-GnFPPKDIVzl3p0BF4zVnrQ"
datadir    = "D:\\Users\\Daria\\Code2\\FP-2016\\CW2\\task1\\.stack-work\\install\\4338eac8\\share\\x86_64-windows-ghc-8.0.1\\task1-0.1.0.0"
libexecdir = "D:\\Users\\Daria\\Code2\\FP-2016\\CW2\\task1\\.stack-work\\install\\4338eac8\\libexec"
sysconfdir = "D:\\Users\\Daria\\Code2\\FP-2016\\CW2\\task1\\.stack-work\\install\\4338eac8\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "task1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "task1_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "task1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "task1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "task1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
