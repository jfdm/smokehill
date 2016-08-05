module Smokehill.Options where

import Options.Applicative

optParser :: Parser Command
optParser = subparser (
     (command "list"
      (info cmdInstalled
            (progDesc "List installed packages.")))
  <> (command "search"
      (info cmdSearch
            (progDesc "Search for packages.")))
  <> (command "show"
      (info cmdShow
            (progDesc "Show information for the given package.")))
  <> (command "install"
      (info cmdInstall
            (progDesc "Try to install the given package.")))
  <> (command "cleanup"
      (info cmdCleanup
            (progDesc "Clean the cache.")))
  <> (command "paths"
      (info cmdPaths
            (progDesc "Show Idris & Smokehill Paths.")))

  )
    where
      cmdInstalled :: Parser Command
      cmdInstalled = pure CMDInstalled

      cmdSearch :: Parser Command
      cmdSearch = CMDSearch <$> argument str (metavar "PKG")

      cmdShow :: Parser Command
      cmdShow = CMDShow <$> argument str (metavar "PKG")

      cmdInstall :: Parser Command
      cmdInstall = CMDInstall
        <$> argument str (metavar "PKG")
        <*> switch (long "dry-run" <> help "Do not try to install")

      cmdCleanup :: Parser Command
      cmdCleanup = CMDCleanup <$> switch (long "force" <> help "Perform cleanup of cache")

      cmdPaths :: Parser Command
      cmdPaths = pure CMDPaths


getOpMode :: IO Command
getOpMode = execParser opts
  where
    opts = info (helper <*> optParser)
                (fullDesc
                   <> progDesc "A simple package manager for Idris."
                   <> header "smokehill")



data Command = CMDInstalled            -- ^ List installed packages
             | CMDSearch String        -- ^ Search for package
             | CMDShow   String        -- ^ Show package info
             | CMDInstall String Bool  -- ^ Try to install package, bool dry run.
             | CMDCleanup Bool         -- ^ Clean cache, bool to do clean
             | CMDPaths                -- ^ Show paths
