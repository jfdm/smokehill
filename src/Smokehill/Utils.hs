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

import Smokehill.Model
import Smokehill.URL

import Paths_smokehill

searchPackages :: String -> Smokehill (Maybe PkgDesc)
searchPackages str = do
  libs <- getLibrary
  let res = find (\pkg -> str == pkgname pkg) libs
  pure res

loadLibrary :: IO [PkgDesc]
loadLibrary = do
  ddir <- getDataDir
  ps <- listDirectory $ ddir </> "packagedb"
  let ps' = filter (\x -> takeExtension x == ".ipkg") ps
  ps'' <- mapM (\x -> makeAbsolute (ddir </> "packagedb" </> x)) ps'
  mapM getPkgDesc ps''

getCacheDirectory :: Smokehill FilePath
getCacheDirectory = do
  cdir <- runIO $ getXdgDirectory XdgCache "smokehill"
  runIO $ createDirectoryIfMissing True cdir
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
