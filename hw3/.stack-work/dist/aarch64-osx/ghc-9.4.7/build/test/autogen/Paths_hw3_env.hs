{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hw3_env (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/noalahan/proglang/hw3/.stack-work/install/aarch64-osx/3f3d8ed7bed333209106ce7396fd8efd525956f5a23d6f7ff9ae36b407d365bf/9.4.7/bin"
libdir     = "/Users/noalahan/proglang/hw3/.stack-work/install/aarch64-osx/3f3d8ed7bed333209106ce7396fd8efd525956f5a23d6f7ff9ae36b407d365bf/9.4.7/lib/aarch64-osx-ghc-9.4.7/hw3-env-0.1.0.0-9lNEjbyGarC5g533dxoOqI-test"
dynlibdir  = "/Users/noalahan/proglang/hw3/.stack-work/install/aarch64-osx/3f3d8ed7bed333209106ce7396fd8efd525956f5a23d6f7ff9ae36b407d365bf/9.4.7/lib/aarch64-osx-ghc-9.4.7"
datadir    = "/Users/noalahan/proglang/hw3/.stack-work/install/aarch64-osx/3f3d8ed7bed333209106ce7396fd8efd525956f5a23d6f7ff9ae36b407d365bf/9.4.7/share/aarch64-osx-ghc-9.4.7/hw3-env-0.1.0.0"
libexecdir = "/Users/noalahan/proglang/hw3/.stack-work/install/aarch64-osx/3f3d8ed7bed333209106ce7396fd8efd525956f5a23d6f7ff9ae36b407d365bf/9.4.7/libexec/aarch64-osx-ghc-9.4.7/hw3-env-0.1.0.0"
sysconfdir = "/Users/noalahan/proglang/hw3/.stack-work/install/aarch64-osx/3f3d8ed7bed333209106ce7396fd8efd525956f5a23d6f7ff9ae36b407d365bf/9.4.7/etc"

getBinDir     = catchIO (getEnv "hw3_env_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hw3_env_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hw3_env_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hw3_env_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw3_env_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw3_env_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
