module Smokehill.API where

import Control.Monad

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Exit

import Data.List
import Data.Maybe

import Idris.Imports (installedPackages)

import Idris.Package.Common
import Idris.Package

import Smokehill.Model
import Smokehill.DVCS
import Smokehill.Dependency
import Smokehill.Utils


showPaths :: Smokehill ()
showPaths = do
  cdir <- getCacheDirectory
  pdir <- getPackageDB
  ldir <- getLibDir

  sPutStrLn "Smokehill Paths"
  sPutWordsLn ["--> Cache Directory:", cdir]
  sPutWordsLn ["--> iPKG Database:", pdir]
  sPutStrLn "Idris Paths"
  sPutWordsLn ["--> Library Directory:", ldir]

cleanCache :: Bool -> Smokehill ()
cleanCache forReal = do
  when (not forReal) $ sPutStrLn "To clean cache for real please use '--force'."
  cdir <- getCacheDirectory
  if forReal
    then runIO $ removeDirectoryRecursive cdir
    else sPutWordsLn ["If called, I would remove:", show cdir]

searchForPackage :: String -> Smokehill ()
searchForPackage pkg = do
  libs <- getLibrary
  let res = filter (pkgSearch pkg) libs
  case res of
    []   -> sPutWordsLn ["Package not found", pkg]
    pkgs -> mapM_ (\x -> sPutWordsLn ["-->", pkgname x]) pkgs

showPackage :: String -> Smokehill ()
showPackage pkg = do
  res <- searchPackages pkg
  case res of
    Nothing  -> sPutWordsLn ["Package not found", pkg]
    Just pkg -> printPrettyPkgDesc pkg

listInstalled :: Smokehill ()
listInstalled = do
  pkgs <- runIO installedPackages
  mapM_ sPutStrLn pkgs

installPackage :: String -> Bool -> Smokehill()
installPackage pkg dryrun = do
  res <- searchPackages pkg
  case res of
    Nothing    -> sPutStrLn "Package doesn't exist in repo."
    Just ipkg  -> do
      ps <- runIO $ installedPackages
      case find (\x -> (pkgname ipkg) == x) ps of
        Just _  -> sPutStrLn "Package already installed."
        Nothing -> do
          sPutWordsLn ["Installing", pkgname ipkg]
          ds <- getInstallOrder ipkg
          case ds of
            [] -> doInstallPackage ipkg dryrun
            is -> do
                sPutWordsLn ["Installing Packages"]
                let ds' = filter (\x -> not $ (pkgname x) `elem` ps) ds
                mapM_ (\x -> doInstallPackage x dryrun) ds'

doInstallPackage :: PkgDesc -> Bool -> Smokehill ()
doInstallPackage ipkg dryrun = do
  sPutWordsLn ["Attempting to install:", pkgname ipkg]
  cdir <- getCacheDirectory
  pkg' <- runIO $ makeAbsolute (cdir </> (pkgname ipkg))
  d    <- runIO $ doesDirectoryExist pkg'
  let pfile = pkg' </> (pkgname ipkg) -<.> "ipkg"
  case (pkgsourceloc ipkg) of
    Nothing   -> sPutWordsLn ["Source Location not specified"]
    Just sloc -> do
      case whichDVCS sloc of
        Nothing   -> sPutWordsLn ["Package is not a valid DVCS url",  sloc]
        Just dvcs -> do
          if d
            then do
              sPutWordsLn ["Directory already exists, checking for updates."]
              errno <- runIO $ dvcsUpdate dvcs pkg'
              case errno of
                ExitFailure _     -> runIO $ exitWith errno
                ExitSuccess       -> do
                  sPutWordsLn ["Attempting to install using:", pfile]
                  when (not dryrun) $ runIO $ buildPkg [] True (True, pfile)
            else do
              sPutWordsLn ["Directory doesn't exists, cloning."]
              sPutWordsLn ["Cloning Git Repo"]
              when (not dryrun) $ do
                errno <- runIO $ dvcsClone dvcs cdir (pkgname ipkg)
                case errno of
                  ExitFailure _     -> runIO $ exitWith errno
                  ExitSuccess       -> do
                    sPutWordsLn ["Attempting to install using:", pfile]
                    runIO $ withCurrentDirectory pkg' $ do
                      when (not dryrun) $ buildPkg [] True (True, pfile)
