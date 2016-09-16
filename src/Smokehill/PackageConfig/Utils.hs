
module Smokehill.PackageConfig.Utils where

import System.Exit
import System.FilePath

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:?), (.:), ToJSON(..), (.=), object)
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.ByteString.Char8 as C

import Smokehill.Model
import Smokehill.DVCS
import Smokehill.IPackage
import Smokehill.PackageConfig

configFromFileIO :: FilePath -> IO (PackageConfig)
configFromFileIO fname = do
  res <- Y.decodeFileEither fname
  case res of
    Left err -> dieIO $ do
        putStrLn "Malformed Yaml file."
        putStrLn $ Y.prettyPrintParseException err
    Right x -> return x

configToFileIO :: FilePath -> PackageConfig -> IO ()
configToFileIO fp = Y.encodeFile (fp <.> "yaml")

configFromFile :: FilePath -> Smokehill (PackageConfig)
configFromFile = runIO . configFromFileIO

configToFile :: FilePath -> PackageConfig -> Smokehill ()
configToFile fp cfg = runIO $ configToFileIO fp cfg

convertFromIPkgIO :: IPackage -> IO PackageConfig
convertFromIPkgIO ipkg = do
    let n = pkgname ipkg
    let d = pkgdeps ipkg
    let b = (pkgbrief ipkg)
    let l = (pkglicense ipkg)
    let h = (pkghomepage ipkg)
    let i = (pkgbugtracker ipkg)
    let v = (pkgversion ipkg)

    s  <- maybeThere (pkgsourceloc ipkg) "DVCS URL is missing, it is required."
    s' <- maybeThere (whichDVCS s)       "Malformed DVCS URL specified."

    return $ PackageConfig n d b v l h s' i
  where
    maybeThere :: Maybe a -> String -> IO a
    maybeThere Nothing  s = failIO s
    maybeThere (Just x) _ = pure x

convertFromIPkg :: IPackage -> Smokehill PackageConfig
convertFromIPkg = runIO . convertFromIPkgIO

displayConfig :: PackageConfig -> Smokehill ()
displayConfig = runIO . C.putStrLn . Y.encode
