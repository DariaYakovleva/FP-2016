{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_coin (
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

bindir     = "D:\\Users\\Daria\\Code2\\FP-2016\\Homework3\\task1_Coins\\coin\\.stack-work\\install\\26812796\\bin"
libdir     = "D:\\Users\\Daria\\Code2\\FP-2016\\Homework3\\task1_Coins\\coin\\.stack-work\\install\\26812796\\lib\\x86_64-windows-ghc-8.0.1\\coin-0.1.0.0-B5zXSSJmLk7Lb6NlVq7XjN"
datadir    = "D:\\Users\\Daria\\Code2\\FP-2016\\Homework3\\task1_Coins\\coin\\.stack-work\\install\\26812796\\share\\x86_64-windows-ghc-8.0.1\\coin-0.1.0.0"
libexecdir = "D:\\Users\\Daria\\Code2\\FP-2016\\Homework3\\task1_Coins\\coin\\.stack-work\\install\\26812796\\libexec"
sysconfdir = "D:\\Users\\Daria\\Code2\\FP-2016\\Homework3\\task1_Coins\\coin\\.stack-work\\install\\26812796\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "coin_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "coin_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "coin_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "coin_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "coin_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
