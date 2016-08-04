module Smokehill.Utils where

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Exit

import Data.List
import Data.Maybe

import Idris.Package
import Idris.Package.Common

import Paths_smokehill

getPkgFileLoc :: String -> IO String
getPkgFileLoc fn = do
  ddir <- getDataDir
  pure (ddir </> "packagedb" </> fn -<.> "ipkg")

getPkgFileContents :: String -> IO String
getPkgFileContents fn = do
  ddir <- getDataDir
  desc <- readFile (ddir </> "packagedb" </> fn -<.> "ipkg")
  pure desc

getPkgFile :: String -> IO PkgDesc
getPkgFile fn = do
  pfileloc <- getPkgFileLoc fn
  getPkgDesc pfileloc

searchPackages :: String -> IO (Maybe FilePath)
searchPackages str = do
  ddir <- getDataDir
  let pdb = ddir </> "packagedb"
  es <- listDirectory pdb
  let es' = map dropExtensions es
  let res = find (str==) es'
  pure res


getCacheDirectory :: IO FilePath
getCacheDirectory = do
  cdir <- getXdgDirectory XdgCache "smokehill"
  createDirectoryIfMissing True cdir
  pure cdir

cloneGitRepo :: FilePath -> String -> FilePath -> IO ExitCode
cloneGitRepo dest loc pkg = do
    dest' <- makeAbsolute dest
    withCurrentDirectory dest $ do
      clone pkg loc
  where
    -- TODO better err reporting
    clone :: String -> String -> IO ExitCode
    clone pkg loc = do
      res <- findExecutable "git"
      case res of
        Nothing -> do
          putStrLn "Git not installed."
          pure $ ExitFailure (-1)
        Just _  -> do
          putStrLn $ unwords ["git clone", loc, pkg]
          createDirectoryIfMissing True pkg
          errno <- rawSystem "git" ["clone", loc, pkg]
          pure errno
