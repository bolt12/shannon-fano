{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_shannon_fano (
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

bindir     = "/home/bolt/.cabal/bin"
libdir     = "/home/bolt/.cabal/lib/x86_64-linux-ghc-8.6.1/shannon-fano-0.1.0.0-EGzhx2vubbrIGLLw9Ypvjf"
dynlibdir  = "/home/bolt/.cabal/lib/x86_64-linux-ghc-8.6.1"
datadir    = "/home/bolt/.cabal/share/x86_64-linux-ghc-8.6.1/shannon-fano-0.1.0.0"
libexecdir = "/home/bolt/.cabal/libexec"
sysconfdir = "/home/bolt/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "shannon_fano_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "shannon_fano_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "shannon_fano_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "shannon_fano_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shannon_fano_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shannon_fano_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
