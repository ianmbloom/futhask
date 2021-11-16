{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_futhask (
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
version = Version [0,2,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/manifold1/.cabal/bin"
libdir     = "/home/manifold1/.cabal/lib/x86_64-linux-ghc-8.6.5/futhask-0.2.0-inplace"
dynlibdir  = "/home/manifold1/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/manifold1/.cabal/share/x86_64-linux-ghc-8.6.5/futhask-0.2.0"
libexecdir = "/home/manifold1/.cabal/libexec/x86_64-linux-ghc-8.6.5/futhask-0.2.0"
sysconfdir = "/home/manifold1/.cabal/etc"

getBinDir     = catchIO (getEnv "futhask_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "futhask_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "futhask_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "futhask_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "futhask_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "futhask_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir ++ fname
  | otherwise                  = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
