module Main where

import System.Exit ( exitSuccess )

import Smokehill

main :: IO ()
main = do
  m <- getOpMode
  case m of
    CMDInstalled     -> listInstalled
    CMDSearch  pkg   -> searchForPackage pkg
    CMDShow    pkg   -> showPackage pkg
    CMDInstall pkg b -> installPackage pkg b
    CMDCleanup b     -> cleanCache b
  exitSuccess
