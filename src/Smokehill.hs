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
import Smokehill.PackageDesc

import Utils

smokehillMain :: IO ()
smokehillMain = runMain $ do
  (Option usr_iexe cmd) <- runIO $ getOpMode

  lib  <- runIO $ loadLibrary
  iexe <- runIO $ getSystemIdrisIO

  setLibrary lib

  setIdrisExe $ fromMaybe iexe usr_iexe

  case cmd of
    (CMDInstalled)       -> listInstalled
    (CMDSearch  pkg)     -> searchForPackage pkg
    (CMDShow    pkg)     -> showPackage pkg
    (CMDInstall pkg d f) -> installPackage pkg d f
    (CMDCleanup b)       -> cleanCache b
    (CMDPaths)           -> showPaths

  runIO $ exitSuccess
