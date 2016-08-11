module Smokehill.Utils where

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Exit

import Data.List
import Data.Maybe
import Data.Char

import Smokehill.Model
import Smokehill.PackageDesc
import Smokehill.PackageDesc.Parser
import Smokehill.Idris

import Data.Version

import Paths_smokehill


getSmokehillVersion = showVersion version 

getSystemIdrisIO :: IO FilePath
getSystemIdrisIO = do
  exe <- findExecutable "idris"
  case exe of
    Nothing -> do
      putStrLn "Idris not installed on PATH."
      exitFailure
    Just exeloc -> do
      pure exeloc

searchPackages :: String -> Smokehill (Maybe PackageDesc)
searchPackages str = do
  libs <- getLibrary
  let res = find (\pkg -> str == pkgname pkg) libs
  pure res

loadLibrary :: IO [PackageDesc]
loadLibrary = do
    pdir <- getPackageDBIO
    ps   <- listDirectory pdir
    let ps'  = filter (correctExt) ps
    let ps'' = map (pdir </>) ps'
    mapM parsePkgDescFile ps''

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

printPrettyPackageDesc :: PackageDesc -> Smokehill ()
printPrettyPackageDesc ipkg = do
    sPutWordsLn ["Name:\t",    pkgname ipkg]
    sPutWordsLn ["Version:\t", fromMaybe "Not Provided" $ pkgversion ipkg]
    sPutWordsLn ["Brief:\t",   fromMaybe "Not Provided" $ pkgbrief ipkg]
    sPutWordsLn ["WWW:\t",     fromMaybe "Not Provided" $ pkghomepage ipkg]
    sPutWordsLn ["DCVS:\t",    fromMaybe "Not Provided" $ pkgsourceloc ipkg]
    sPutWordsLn ["Deps:\t",    show (pkgdeps ipkg)]

pkgSearch :: String -> PackageDesc -> Bool
pkgSearch x ipkg = (lowerName x) `isInfixOf` (lowerName $ pkgname ipkg)
  where
    lowerName :: String -> String
    lowerName = map toLower

correctExt :: String -> Bool
correctExt s = takeExtension s == ".ipkg"
