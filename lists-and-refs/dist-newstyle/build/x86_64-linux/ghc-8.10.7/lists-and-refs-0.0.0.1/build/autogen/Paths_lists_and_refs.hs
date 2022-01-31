{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_lists_and_refs (
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
version = Version [0,0,0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/stevensonmat2/.cabal/bin"
libdir     = "/home/stevensonmat2/.cabal/lib/x86_64-linux-ghc-8.10.7/lists-and-refs-0.0.0.1-inplace"
dynlibdir  = "/home/stevensonmat2/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/stevensonmat2/.cabal/share/x86_64-linux-ghc-8.10.7/lists-and-refs-0.0.0.1"
libexecdir = "/home/stevensonmat2/.cabal/libexec/x86_64-linux-ghc-8.10.7/lists-and-refs-0.0.0.1"
sysconfdir = "/home/stevensonmat2/.cabal/etc"

getBinDir     = catchIO (getEnv "lists_and_refs_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "lists_and_refs_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "lists_and_refs_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "lists_and_refs_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lists_and_refs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lists_and_refs_sysconfdir") (\_ -> return sysconfdir)




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
