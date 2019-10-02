{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_quickCheckExamples (
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

bindir     = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch14\\quickCheckExamples\\.stack-work\\install\\d1e81147\\bin"
libdir     = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch14\\quickCheckExamples\\.stack-work\\install\\d1e81147\\lib\\x86_64-windows-ghc-8.4.3\\quickCheckExamples-0.1.0-5ZTpO4foyUP2JNM6UJFmvb"
dynlibdir  = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch14\\quickCheckExamples\\.stack-work\\install\\d1e81147\\lib\\x86_64-windows-ghc-8.4.3"
datadir    = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch14\\quickCheckExamples\\.stack-work\\install\\d1e81147\\share\\x86_64-windows-ghc-8.4.3\\quickCheckExamples-0.1.0"
libexecdir = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch14\\quickCheckExamples\\.stack-work\\install\\d1e81147\\libexec\\x86_64-windows-ghc-8.4.3\\quickCheckExamples-0.1.0"
sysconfdir = "D:\\SLC\\functionalProgramming\\haskellProgramming\\ch14\\quickCheckExamples\\.stack-work\\install\\d1e81147\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "quickCheckExamples_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "quickCheckExamples_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "quickCheckExamples_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "quickCheckExamples_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quickCheckExamples_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quickCheckExamples_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
