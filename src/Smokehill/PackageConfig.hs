{-# LANGUAGE OverloadedStrings #-}
module Smokehill.PackageConfig where

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:?), (.:), ToJSON(..), (.=), object)

import Smokehill.DVCS

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
