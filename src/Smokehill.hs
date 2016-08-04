module Smokehill
  ( module Smokehill.Options
    -- * API
  , listInstalled
  , searchForPackage
  , showPackage
  , installPackage
  , cleanCache
  ) where

import Control.Monad

import System.Directory
import System.FilePath
import System.IO
import System.Exit
import System.Process

import Data.List
import Data.Maybe

import Network.URL (importURL)

import Idris.Imports (installedPackages)

import Idris.Package.Common
import Idris.Package

import Smokehill.Options
import Smokehill.Dependency
import Smokehill.URL
import Smokehill.Utils

import Utils

listInstalled :: IO ()
listInstalled = do
  pkgs <- installedPackages
  mapM_ putStrLn pkgs


searchForPackage :: String -> IO ()
searchForPackage pkg = do
  res <- searchPackages pkg
  print $ fromMaybe "Package doesn't exist" res


showPackage :: String -> IO ()
showPackage pkg = do
  res <- searchPackages pkg
  case res of
    Nothing   -> putStrLn "Package doesn't exist"
    Just pkg' -> do
      desc <- getPkgFileContents pkg'
      putStrLn desc

installPackage :: String -> Bool -> IO ()
installPackage pkg dryrun = do
  res <- searchPackages pkg
  case res of
    Nothing -> putStrLn "Package doesn't exist in repo."
    Just _  -> do
      ps <- installedPackages
      case find (pkg==) ps of
        Just _  -> putStrLn "Package already installed."
        Nothing -> do
          putStrLnWs ["Installing", pkg]
          ipkg <- getPkgFile pkg
          ds <- getInstallOrder ipkg
          case ds of
            [] -> doInstallPackage pkg dryrun
            is -> do
              ideps <- mapM searchPackages is
              if any isNothing ideps
                then do
                  putStrLn "Some packages don't exist"
                  mapM_ (\x -> putStrLn $ show x) ideps
                else do
                  putStrLnWs ["Installing Packages"]
                  ithere <- installedPackages
                  let ds' = ds \\ ithere
                  mapM_ (\x -> doInstallPackage x dryrun) ds'

doInstallPackage :: String -> Bool -> IO ()
doInstallPackage pkg dryrun = do
  putStrLnWs ["Attempting to install:", pkg]
  ipkg <- getPkgFile pkg
  case (pkgsourceloc ipkg) of
    Nothing   -> putStrLnWs ["Source Location not specified"]
    Just sloc -> do
      if (not $ isGit sloc)
        then putStrLnWs ["Package is not a valid url",  sloc]
        else do
          cdir <- getCacheDirectory
          pkg' <- makeAbsolute (cdir </> pkg)
          d    <- doesDirectoryExist pkg'
          when d $ do
            putStrLnWs ["Directory already exists, skipping."]
          when (not dryrun) $ do
            putStrLnWs ["Cloning Git Repo"]
            errno <- cloneGitRepo cdir (drop 6 sloc) pkg
            case errno of
              ExitFailure _     -> exitWith errno
              ExitSuccess       -> do
                withCurrentDirectory pkg' $ do
                  let pfile = pkg' </> pkg -<.> "ipkg"
                  putStrLnWs ["Installing", show pfile]
                  when (not dryrun) $ buildPkg [] True (True, pfile)

cleanCache :: Bool -> IO ()
cleanCache forReal = do
  when (not forReal) $ putStrLnWs ["To clean cache for real please use '--force'."]
  cdir <- getCacheDirectory
  if forReal
    then removeDirectoryRecursive cdir
    else putStrLnWs ["If called, I would remove:", show cdir]
