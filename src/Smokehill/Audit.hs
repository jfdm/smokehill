module Smokehill.Audit
  (
    auditIPackage
  ) where

import Data.List

import Smokehill.Model
import Smokehill.IPackage
import Smokehill.IPackage.Parser
import Smokehill.Idris
import Smokehill.Utils
import Smokehill.DVCS

auditIPackage :: FilePath -> Smokehill ()
auditIPackage fp = do
  pkg <- runIO $ parsePkgDescFile fp

  sPutWordsLn $ ["Loading of", fp, "successfull."]

  sPutStrLn "Searching for required fields..."
  printPrettyIPackage pkg

  sPutStrLn "Checking validity of required fields..."

  case (pkgsourceloc pkg) of
    Nothing -> do
      sPutStrLn "A valid DVCS URI must be specified."
      sPutStrLn "URI must be of the form:"
      sPutStrLn "\t <git|hg>://<repo url>"
    Just sloc -> do
      case whichDVCS sloc of
        Nothing -> do
          sPutStrLn "Invalid DVCS URI specified."
          sPutStrLn "URI must be of the form:"
          sPutStrLn "\t <git|hg>://<repo url>"
        Just _ -> do
          sPutStrLn "Source location is valid."

  if pkgSpecifiedInOptions pkg
    then do
      sPutStrLn "Package dependencies must not be specified in the options string."
      sPutStrLn "Please use the `pkgs=` field of the IPKG format."
    else do
      sPutStrLn "Good that no package dependencies were specified in the options string."


pkgSpecifiedInOptions :: IPackage -> Bool
pkgSpecifiedInOptions pkg = result
  where
    result = short_res || long_res
    short_res = isInfixOf "-p"  (idris_opts pkg)
    long_res  = isInfixOf "--package"  (idris_opts pkg)
