module Smokehill.Utils where

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Exit

import Data.List
import Data.Maybe
import Data.Char

import IRTS.System (getIdrisLibDir)

import Idris.Package
import Idris.Package.Common

import Smokehill.Model
import Smokehill.URL

import Paths_smokehill


getLibDir :: Smokehill FilePath
getLibDir = runIO $ getIdrisLibDir

searchPackages :: String -> Smokehill (Maybe PkgDesc)
searchPackages str = do
  libs <- getLibrary
  let res = find (\pkg -> str == pkgname pkg) libs
  pure res

loadLibrary :: IO [PkgDesc]
loadLibrary = do
    pdir <- getPackageDBIO
    ps   <- listDirectory pdir
    let ps'  = filter (correctExt) ps
    let ps'' = map (pdir </>) ps'
    mapM getPkgDesc ps''

getPackageDBIO :: IO FilePath
getPackageDBIO = do
  ddir <- getDataDir
  pdir <- makeAbsolute (ddir </> "packagedb")
  pure pdir

getPackageDB :: Smokehill FilePath
getPackageDB = runIO $ getPackageDBIO

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

printPrettyPkgDesc :: PkgDesc -> Smokehill ()
printPrettyPkgDesc ipkg = do
    sPutWordsLn ["Name:\t",    pkgname ipkg]
    sPutWordsLn ["Version:\t", fromMaybe "Not Provided" $ pkgversion ipkg]
    sPutWordsLn ["Brief:\t",   fromMaybe "Not Provided" $ pkgbrief ipkg]
    sPutWordsLn ["WWW:\t",     fromMaybe "Not Provided" $ pkghomepage ipkg]
    sPutWordsLn ["DCVS:\t",    fromMaybe "Not Provided" $ pkgsourceloc ipkg]
    sPutWordsLn ["Deps:\t",    show (pkgdeps ipkg)]

pkgSearch :: String -> PkgDesc -> Bool
pkgSearch x ipkg = (lowerName x) `isInfixOf` (lowerName $ pkgname ipkg)
  where
    lowerName :: String -> String
    lowerName = map toLower

correctExt :: String -> Bool
correctExt s = takeExtension s == ".ipkg"
