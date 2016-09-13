{-# LANGUAGE OverloadedStrings #-}

module Smokehill.Settings
  (
    SSettings(..)
  , settingsFromFile
  ) where

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), ToJSON(..), (.=), object)
import Control.Applicative
import Smokehill.DVCS

data SSettings = SSettings
  {
    ipkg_repo :: DVCS
  , idris_path :: FilePath
  }

instance ToJSON SSettings where
  toJSON (SSettings r p) = object [ "packagedb" .= r
                                  , "idrispath" .= p]

instance FromJSON SSettings where
  parseJSON (Y.Object v) =
        SSettings
    <$> v .: "packagedb"
    <*> v .: "idrispath"
  parseJSON _ = fail "Expected Object for Config value"


settingsFromFile :: FilePath -> IO (Maybe SSettings)
settingsFromFile = Y.decodeFile
