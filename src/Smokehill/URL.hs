module Smokehill.URL where

import Data.List (isPrefixOf, stripPrefix)

isGit :: String -> Bool
isGit = isPrefixOf "git://"

stripGit :: String -> Maybe String
stripGit = stripPrefix "git://"

stripHG :: String -> Maybe String
stripHG = stripPrefix "hg://"
