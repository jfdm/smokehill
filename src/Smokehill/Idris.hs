module Smokehill.Idris
  (
    idrisInstall
  , idrisLibDir
  , idrisPkgs
  , idrisExe
  ) where

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Exit

import Data.List (isPrefixOf, stripPrefix)

import Smokehill.Model

import Utils


doIdris :: String -> List String -> IO (ExitCode, String, String)
doIdris exe args = do
  let iproc = proc exe args
  putStrLn $ unwords ([exe] ++ args)
  readCreateProcessWithExitCode iproc []

idrisInstall :: FilePath
             -> String
             -> Smokehill ()
idrisInstall dir ipkg = do
  iexe <- getIdrisExe
  runIO $ do
    dir' <- makeAbsolute dir
    withCurrentDirectory dir' $ do
      putStrLn $ unwords [iexe, "--install", ipkg]
      callProcess iexe ["--install", ipkg]


idrisLibDir :: Smokehill FilePath
idrisLibDir = do
  iexe <- getIdrisExe
  (ecode, std_out, _) <- runIO $ doIdris iexe ["--libdir"]
  case ecode of
    (ExitFailure _) -> runIO $ exitWith ecode
    ExitSuccess     -> pure $ (head . lines) std_out

idrisPkgs :: Smokehill (List String)
idrisPkgs = do
  iexe <- getIdrisExe
  (ecode, std_out, _) <- runIO $ doIdris iexe ["--listlibs"]
  case ecode of
    (ExitFailure _) -> runIO $ exitWith ecode
    ExitSuccess     -> pure  $ lines std_out

idrisExe :: Smokehill FilePath
idrisExe = getIdrisExe
