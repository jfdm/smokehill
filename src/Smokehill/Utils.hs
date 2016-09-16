module Smokehill.Utils where

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Exit

import Data.List
import Data.Maybe
import Data.Char

import qualified Data.Yaml as Y

import Smokehill.Model
import Smokehill.IPackage
import Smokehill.IPackage.Parser
import Smokehill.Idris
import Smokehill.Settings
import Data.Version

import Paths_smokehill

getSettingsLocationIO :: IO FilePath
getSettingsLocationIO = do
  ddir <- getSmokehillConfigDirIO
  sp <- makeAbsolute (ddir </> "settings.yaml")
  pure sp

getSettingsLocation :: Smokehill FilePath
getSettingsLocation = runIO $ getSettingsLocationIO

getSettings :: Smokehill SSettings
getSettings = do
  defSettings <- mkDefaultSettings
  runIO $ (getSettingsIO defSettings)

getSettingsIO :: SSettings -> IO SSettings
getSettingsIO defSet = do
  sp <- getSettingsLocationIO
  exists <- (doesFileExist sp)
  if not exists
    then do
      putStrLn "settings.yaml doesn't exist."
      putStrLn "Writing default settings"
      settingsToFile defSet
      pure defSet
    else do
      sp' <- settingsFromFile sp
      case  sp' of
           Nothing -> do
             putStrLn "settings.yaml is corrupted."
             putStrLn "Writing default settings."
             settingsToFile defSet
             pure defSet
           Just s -> pure s


mkDefaultSettings :: Smokehill SSettings
mkDefaultSettings = do
  iexe <- runIO $ getSystemIdrisIO
  repo <- getPackageRepo
  pure $ SSettings repo iexe


settingsToFile :: SSettings -> IO ()
settingsToFile s = do
    sp <- getSettingsLocationIO
    Y.encodeFile sp s

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

searchPackages :: String -> Smokehill (Maybe IPackage)
searchPackages str = do
  libs <- getLibrary
  let res = find (\pkg -> str == pkgname pkg) libs
  pure res

loadLibrary :: IO [IPackage]
loadLibrary = do
    pdir <- getPackageDBIO

    ps   <- listDirectory pdir
    case filter (correctExtIPKG) ps of
      [] -> do
        putStrLn "Package DB is empty."
        pure []
      ps' -> do
        let ps'' = map (pdir </>) ps'
        mapM parsePkgDescFileIO ps''

getPackageDBIO :: IO FilePath
getPackageDBIO = do
  ddir <- getSmokehillDataDirIO
  pdir <- makeAbsolute (ddir </> "packagedb")
  createDirectoryIfMissing True pdir
  pure pdir

getPackageDB :: Smokehill FilePath
getPackageDB = runIO $ getPackageDBIO

getSmokehillDataDirIO :: IO FilePath
getSmokehillDataDirIO = do
  ddir <- getXdgDirectory XdgData "smokehill"
  createDirectoryIfMissing True ddir
  pure ddir

getSmokehillDataDir :: Smokehill FilePath
getSmokehillDataDir = runIO $ getSmokehillDataDirIO

getSmokehillCacheDir :: Smokehill FilePath
getSmokehillCacheDir = do
  cdir <- runIO $ getXdgDirectory XdgCache "smokehill"
  runIO $ createDirectoryIfMissing True cdir
  pure cdir

getSmokehillConfigDirIO :: IO FilePath
getSmokehillConfigDirIO = do
  ddir <- getXdgDirectory XdgConfig "smokehill"
  createDirectoryIfMissing True ddir
  pure ddir

getSmokehillConfigDir :: Smokehill FilePath
getSmokehillConfigDir = runIO $ getSmokehillConfigDirIO

printPrettyIPackage :: IPackage -> Smokehill ()
printPrettyIPackage ipkg = do
    sPutWordsLn ["Name:\t",    pkgname ipkg]
    sPutWordsLn ["Version:\t", fromMaybe "Not Provided" $ pkgversion ipkg]
    sPutWordsLn ["Brief:\t",   fromMaybe "Not Provided" $ pkgbrief ipkg]
    sPutWordsLn ["WWW:\t",     fromMaybe "Not Provided" $ pkghomepage ipkg]
    sPutWordsLn ["DCVS:\t",    fromMaybe "Not Provided" $ pkgsourceloc ipkg]
    sPutWordsLn ["Deps:\t",    show (pkgdeps ipkg)]

pkgSearch :: String -> IPackage -> Bool
pkgSearch x ipkg = (lowerName x) `isInfixOf` (lowerName $ pkgname ipkg)
  where
    lowerName :: String -> String
    lowerName = map toLower

correctExt :: String -> String -> Bool
correctExt e s = takeExtension s == e

correctExtIPKG :: String -> Bool
correctExtIPKG = correctExt ".ipkg"

correctExtYAML :: String -> Bool
correctExtYAML = correctExt ".yaml"
