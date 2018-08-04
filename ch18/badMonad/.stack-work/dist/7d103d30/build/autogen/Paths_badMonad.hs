{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_badMonad (
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

bindir     = "D:\\SLC\\functionalProgramming\\HaskellProgramming\\ch18\\badMonad\\.stack-work\\install\\dbe016db\\bin"
libdir     = "D:\\SLC\\functionalProgramming\\HaskellProgramming\\ch18\\badMonad\\.stack-work\\install\\dbe016db\\lib\\x86_64-windows-ghc-8.4.3\\badMonad-0.1.0.0-6vEAoxdjuPe9WSBtfiY48A"
dynlibdir  = "D:\\SLC\\functionalProgramming\\HaskellProgramming\\ch18\\badMonad\\.stack-work\\install\\dbe016db\\lib\\x86_64-windows-ghc-8.4.3"
datadir    = "D:\\SLC\\functionalProgramming\\HaskellProgramming\\ch18\\badMonad\\.stack-work\\install\\dbe016db\\share\\x86_64-windows-ghc-8.4.3\\badMonad-0.1.0.0"
libexecdir = "D:\\SLC\\functionalProgramming\\HaskellProgramming\\ch18\\badMonad\\.stack-work\\install\\dbe016db\\libexec\\x86_64-windows-ghc-8.4.3\\badMonad-0.1.0.0"
sysconfdir = "D:\\SLC\\functionalProgramming\\HaskellProgramming\\ch18\\badMonad\\.stack-work\\install\\dbe016db\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "badMonad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "badMonad_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "badMonad_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "badMonad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "badMonad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "badMonad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
