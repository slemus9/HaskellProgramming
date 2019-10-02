{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_test (
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
version = Version [0,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch16\\test\\.stack-work\\install\\bf6b262a\\bin"
libdir     = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch16\\test\\.stack-work\\install\\bf6b262a\\lib\\x86_64-windows-ghc-8.4.3\\test-0.1.0-2swz2t2ryqw1sx75VflkfL"
dynlibdir  = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch16\\test\\.stack-work\\install\\bf6b262a\\lib\\x86_64-windows-ghc-8.4.3"
datadir    = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch16\\test\\.stack-work\\install\\bf6b262a\\share\\x86_64-windows-ghc-8.4.3\\test-0.1.0"
libexecdir = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch16\\test\\.stack-work\\install\\bf6b262a\\libexec\\x86_64-windows-ghc-8.4.3\\test-0.1.0"
sysconfdir = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch16\\test\\.stack-work\\install\\bf6b262a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "test_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "test_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "test_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "test_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "test_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "test_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
