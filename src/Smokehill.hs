module Smokehill
  (
  smokehillMain
  ) where

import Control.Monad

import System.Directory
import System.FilePath
import System.IO
import System.Exit
import System.Process

import Data.List
import Data.Maybe

import Smokehill.Model
import Smokehill.Options
import Smokehill.API
import Smokehill.Dependency
import Smokehill.Utils
import Smokehill.IPackage
import Smokehill.Settings

import Utils

smokehillMain :: IO ()
smokehillMain = runMain $ do
  (Option usr_iexe cmd) <- runIO $ getOpMode

  settings <- getSettings

  lib  <- runIO $ loadLibrary
  case lib of
    [] -> do
      updatePackageIndex
      runIO $ exitSuccess
    lib' -> setLibrary lib

  let iexe = idris_path settings

  setIdrisExe $ fromMaybe iexe usr_iexe
  setPackageRepo (ipkg_repo settings)

  case cmd of
    (CMDInstalled)         -> listInstalled
    (CMDSearch  pkg)       -> searchForPackage pkg
    (CMDShow    pkg)       -> showPackage pkg
    (CMDInstall pkg d f i) -> installPackage pkg d f i
    (CMDCleanup b)         -> cleanCache b
    (CMDPaths)             -> showPaths
    (CMDAudit   pkg)       -> auditPackage pkg
    (CMDUpdate)            -> updatePackageIndex
    (CMDConvert pkg f)     -> convertPackageFile pkg f
    (CMDShowDeps pkg)      -> showPackageDependencies pkg
  runIO $ exitSuccess
