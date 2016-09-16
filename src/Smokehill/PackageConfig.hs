{-# LANGUAGE OverloadedStrings #-}
module Smokehill.PackageConfig where

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

data PackageConfig = PackageConfig
  {
    name       :: String
  , deps       :: [String]
  , brief      :: Maybe String
  , version    :: Maybe String
  , license    :: Maybe String
  , homepage   :: Maybe String
  , sourceloc  :: DVCS
  , bugtracker :: Maybe String
  } deriving (Show)


instance ToJSON PackageConfig where
  toJSON config =
    object [ "name"       .= (name config)
           , "packages"   .= (deps config)
           , "brief"      .= (brief config)
           , "version"    .= (version config)
           , "license"    .= (license config)
           , "homepage"   .= (homepage config)
           , "sourceloc"  .= (sourceloc config)
           , "bugtracker" .= (bugtracker config)
           ]

instance FromJSON PackageConfig where
  parseJSON (Y.Object v) =
         PackageConfig
     <$> v .: "name"
     <*> v .: "packages"
     <*> v .:? "brief"
     <*> v .:? "version"
     <*> v .:? "license"
     <*> v .:? "homepage"
     <*> v .: "sourceloc"
     <*> v .:? "bugtracker"
  parseJSON _ = fail "Expected Object for Config value"

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
