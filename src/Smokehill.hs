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

import Idris.Package.Common

import Smokehill.Model
import Smokehill.Options
import Smokehill.API
import Smokehill.Dependency
import Smokehill.Utils

import Utils

smokehillMain :: IO ()
smokehillMain = runMain $ do
  os   <- runIO $ getOpMode
  lib  <- runIO $ loadLibrary
  iexe <- runIO $ getSystemIdrisIO

  setLibrary lib
  setIdrisExe iexe

  parseCmd os

  runIO $ exitSuccess


parseCmd :: Command -> Smokehill ()
parseCmd (CMDInstalled)       = listInstalled
parseCmd (CMDSearch  pkg)     = searchForPackage pkg
parseCmd (CMDShow    pkg)     = showPackage pkg
parseCmd (CMDInstall pkg d f) = installPackage pkg d f
parseCmd (CMDCleanup b)       = cleanCache b
parseCmd (CMDPaths)           = showPaths
