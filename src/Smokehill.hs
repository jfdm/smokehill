module Smokehill
  (
  mainSmokehill
  ) where

import Control.Monad

import System.Directory
import System.FilePath
import System.IO
import System.Exit
import System.Process

import Data.List
import Data.Maybe

import Idris.Package.Common

import Smokehill.Model
import Smokehill.Options
import Smokehill.API
import Smokehill.Dependency
import Smokehill.Utils

import Utils

mainSmokehill :: IO ()
mainSmokehill = do
  os  <- getOpMode
  lib <- loadLibrary
  runMain (theMain os lib)
  exitSuccess

theMain :: Command -> [PkgDesc] -> Smokehill ()
theMain cmd libs = do
  setLibrary libs
  parseCmd cmd

parseCmd :: Command -> Smokehill ()
parseCmd (CMDInstalled)       = listInstalled
parseCmd (CMDSearch  pkg)     = searchForPackage pkg
parseCmd (CMDShow    pkg)     = showPackage pkg
parseCmd (CMDInstall pkg d f) = installPackage pkg d f
parseCmd (CMDCleanup b)       = cleanCache b
parseCmd (CMDPaths)           = showPaths
