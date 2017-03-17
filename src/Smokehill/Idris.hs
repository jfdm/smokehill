module Smokehill.Idris
  (
    idrisInstall
  , idrisLibDir
  , idrisPkgs
  , idrisExe
  ) where

import Control.Monad

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
  readCreateProcessWithExitCode iproc []

idrisInstall :: FilePath
             -> String
             -> Bool
             -> Smokehill ()
idrisInstall dir ipkg docs = do
  iexe <- getIdrisExe
  catchError
    (runIO $ do
      dir' <- makeAbsolute dir
      withCurrentDirectory dir' $ do
        putStrLn $ unwords [iexe, "--install", ipkg]
        callProcess iexe ["--install", ipkg]
        when docs $ do
          putStrLn $ unwords [iexe, "--installdoc", ipkg]
          callProcess iexe ["--installdoc", ipkg])
    (\err -> do
            sPutStrLn "Error when installing package"
            runIO $ exitFailure)

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
