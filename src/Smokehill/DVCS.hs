{-# LANGUAGE OverloadedStrings #-}

module Smokehill.DVCS
  (
    DVCS()
  , whichDVCS
  , dvcsClone
  , dvcsUpdate
  , newGitRepo
  , newHGRepo
  ) where

import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Exit
import Control.Applicative

import Data.Text (Text)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), ToJSON(..), (.=), object)
import Data.List (isPrefixOf, stripPrefix)

import Utils

data DVCS = Git String
          | HG  String
          deriving (Show)

newGitRepo :: String -> DVCS
newGitRepo = Git

newHGRepo :: String -> DVCS
newHGRepo = HG

whichDVCS :: String -> Maybe DVCS
whichDVCS url =
  case stripGit url of
    Just u  -> Just $ Git u
    Nothing ->
      case stripHG url of
        Just u  -> Just $ HG u
        Nothing -> Nothing

stripGit :: String -> Maybe String
stripGit = stripPrefix "git://"

stripHG :: String -> Maybe String
stripHG = stripPrefix "hg://"


doDVCS :: String -> List String -> IO ExitCode
doDVCS exe' args = do
  exe <- findExecutable exe'
  case exe of
    Nothing -> do
      putStrLn $ unwords [exe', "not installed."]
      pure $ ExitFailure (-1)
    Just _  -> do
      putStrLn $ unwords (exe':args)
      errno <- rawSystem exe' args
      pure errno

doClone :: String
        -> String
        -> FilePath
        -> String
        -> IO ExitCode
doClone exe sloc rdir pkg = do
  dest <- makeAbsolute rdir
  withCurrentDirectory dest $ do
    createDirectoryIfMissing True pkg
    doDVCS exe ["clone", sloc, pkg]

dvcsClone :: DVCS
          -> FilePath -- ^ Where repos are to be located
          -> String   -- ^ The folder to call the cloned repo.
          -> IO ExitCode
dvcsClone (Git u) = doClone "git" u
dvcsClone (HG  u) = doClone "hg"  u

dvcsUpdate :: DVCS -> FilePath -> IO ExitCode
dvcsUpdate dvcs loc = do
  rdir <- makeAbsolute loc
  withCurrentDirectory rdir $ do
    case dvcs of
      (Git _) -> doDVCS "git" ["pull"]
      (HG  _) -> doDVCS "hg"  ["pull", "-u"]


instance FromJSON DVCS where
  parseJSON (Y.Object v) =
       newGitRepo <$> v .: "git"
   <|> newHGRepo  <$> v .: "hg"
  parseJSON _ = fail "Expected Object for DVCS option."

instance ToJSON DVCS where
    toJSON (Git u) = object ["git" .= u]
    toJSON (HG  u) = object ["hg"  .= u]
