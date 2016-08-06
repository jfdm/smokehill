module Smokehill.API
  (
    showPaths
  , cleanCache
  , searchForPackage
  , showPackage
  , listInstalled
  , installPackage
  ) where

import Control.Monad

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Exit

import Data.List
import Data.Maybe

import Idris.Package.Common
import Idris.Package

import Smokehill.Model
import Smokehill.DVCS
import Smokehill.Dependency
import Smokehill.Utils
import Smokehill.Idris

showPaths :: Smokehill ()
showPaths = do
  cdir <- getCacheDirectory
  pdir <- getPackageDB
  ldir <- idrisLibDir
  iexe <- idrisExe

  sPutStrLn "Smokehill Paths"
  sPutWordsLn ["--> Cache Directory:", cdir]
  sPutWordsLn ["--> iPKG Database:", pdir]
  sPutStrLn "Idris Paths"
  sPutWordsLn ["--> Idris Exe:", iexe]
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
  pkgs <- idrisPkgs
  mapM_ sPutStrLn pkgs

installPackage :: String -> Bool -> Bool -> Smokehill()
installPackage pkg dryrun force = do
  res <- searchPackages pkg
  case res of
    Nothing    -> sPutStrLn "Package doesn't exist in repo."
    Just ipkg  -> do

      ps <- idrisPkgs

      let there = find ((pkgname ipkg) ==) ps

      if (isJust there) && (not force)
        then sPutStrLn "Package already installed."
        else do
          sPutWordsLn ["Installing", pkgname ipkg]
          sPutWordsLn ["Determining Dependencies"]
          ds <- getInstallOrder ipkg
          case ds of
            [ipkg] -> do
                sPutStrLn "No dependencies"
                performInstall ipkg dryrun
            is -> do
                let ds' = filter (\x -> not $ (pkgname x) `elem` ps) ds
                let ds'' = if force then ds else ds'

                sPutWordsLn $ ["Installing Packages:"] ++ map pkgname ds''
                mapM_ (\x -> performInstall x dryrun) ds''

performInstall :: PkgDesc -> Bool -> Smokehill ()
performInstall ipkg dryrun = do
    sPutWordsLn ["Attempting to install:", pkgname ipkg]
    cdir <- getCacheDirectory
    pdir <- runIO $ makeAbsolute (cdir </> (pkgname ipkg))
    d    <- runIO $ doesDirectoryExist pdir
    case (pkgsourceloc ipkg) of
      Nothing   -> sPutWordsLn ["Source Location not specified"]
      Just sloc -> do
        case whichDVCS sloc of
          Nothing   -> sPutWordsLn ["Package is not a valid DVCS url",  sloc]
          Just dvcs -> do
            if d
              then do
                sPutWordsLn ["Directory already exists, checking for updates."]
                when (not dryrun) $ do
                  errno <- runIO $ dvcsUpdate dvcs pdir
                  doInstall errno pdir ipkg
              else do
                sPutWordsLn ["Directory doesn't exists, cloning."]
                sPutWordsLn ["Cloning Git Repo"]
                when (not dryrun) $ do
                  errno <- runIO $ dvcsClone dvcs cdir (pkgname ipkg)
                  doInstall errno pdir ipkg
  where
    doInstall :: ExitCode -> FilePath -> PkgDesc -> Smokehill ()
    doInstall err@(ExitFailure _) _    _    = runIO $ exitWith err
    doInstall ExitSuccess         pdir ipkg = do
      let pfile = pdir </> (pkgname ipkg) -<.> "ipkg"
      sPutWordsLn ["Attempting to install using:", pfile]
      when (not dryrun) $ do
        idrisInstall pdir pfile
