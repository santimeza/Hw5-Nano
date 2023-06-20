{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw5_types (
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

bindir     = "/workspaces/Hw5-Nano/.stack-work/install/x86_64-linux-tinfo6/3b47b7c8430747e0087761ae794e2bd2406a1f19ca21ee7ae68b923fafa72bf1/8.10.4/bin"
libdir     = "/workspaces/Hw5-Nano/.stack-work/install/x86_64-linux-tinfo6/3b47b7c8430747e0087761ae794e2bd2406a1f19ca21ee7ae68b923fafa72bf1/8.10.4/lib/x86_64-linux-ghc-8.10.4/hw5-types-0.1.0.0-5aJYXbCJlQB720w9YxnKvx-nano"
dynlibdir  = "/workspaces/Hw5-Nano/.stack-work/install/x86_64-linux-tinfo6/3b47b7c8430747e0087761ae794e2bd2406a1f19ca21ee7ae68b923fafa72bf1/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/workspaces/Hw5-Nano/.stack-work/install/x86_64-linux-tinfo6/3b47b7c8430747e0087761ae794e2bd2406a1f19ca21ee7ae68b923fafa72bf1/8.10.4/share/x86_64-linux-ghc-8.10.4/hw5-types-0.1.0.0"
libexecdir = "/workspaces/Hw5-Nano/.stack-work/install/x86_64-linux-tinfo6/3b47b7c8430747e0087761ae794e2bd2406a1f19ca21ee7ae68b923fafa72bf1/8.10.4/libexec/x86_64-linux-ghc-8.10.4/hw5-types-0.1.0.0"
sysconfdir = "/workspaces/Hw5-Nano/.stack-work/install/x86_64-linux-tinfo6/3b47b7c8430747e0087761ae794e2bd2406a1f19ca21ee7ae68b923fafa72bf1/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw5_types_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw5_types_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw5_types_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw5_types_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw5_types_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw5_types_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
