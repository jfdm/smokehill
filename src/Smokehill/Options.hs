module Smokehill.Options where

import Options.Applicative

import Smokehill.Utils (getSmokehillVersion)

optParser :: Parser Option
optParser = Option
    <$> globalOptParser
    <*> cmdParser


globalOptParser :: Parser (Maybe FilePath)
globalOptParser = optional $ strOption (long "custom-idris"
               <> metavar "EXE"
               <> help "Custom Idris executable to use." )

cmdParser  :: Parser Command
cmdParser = subparser (
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
  <> (command "update"
      (info cmdUpdate
            (progDesc "Update package index")))
  <> (command "audit"
      (info cmdAudit
            (progDesc "Check if iPKG file is correct..")))
  <> (command "convert"
      (info cmdConvert
            (progDesc "Generate an Yaml file for inclusion.")))

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
        <*> switch (long "force"   <> help "Force installation")

      cmdCleanup :: Parser Command
      cmdCleanup = CMDCleanup <$> switch (long "force" <> help "Perform cleanup of cache")

      cmdPaths :: Parser Command
      cmdPaths = pure CMDPaths

      cmdUpdate :: Parser Command
      cmdUpdate = pure CMDUpdate

      cmdAudit :: Parser Command
      cmdAudit = CMDAudit
          <$> argument str (metavar "PKG")

      cmdConvert :: Parser Command
      cmdConvert = CMDConvert
          <$> argument str (metavar "PKG")
          <*> (optional $ strOption $
                              long "output"
                           <> short 'o'
                           <> metavar "yaml")

versionFlag = infoOption getSmokehillVersion
                (short 'v'
                <> long "version"
                <> help "Print version information.")

getOpMode :: IO Option
getOpMode = execParser opts
  where
    opts = info (versionFlag <*> helper <*> optParser)
                (fullDesc
                   <> progDesc "A simple package manager for Idris."
                   <> header "smokehill")


data Option = Option (Maybe FilePath) Command

data Command = CMDInstalled                       -- ^ List installed packages
             | CMDSearch String                   -- ^ Search for package
             | CMDShow   String                   -- ^ Show package info
             | CMDInstall String Bool Bool        -- ^ Try to install package, bool dry run.
             | CMDCleanup Bool                    -- ^ Clean cache, bool to do clean
             | CMDPaths                           -- ^ Show paths
             | CMDAudit String                    -- ^ Audit a ipkg file for use in conversion.
             | CMDUpdate                          -- ^ Update ipkg files.
             | CMDConvert String (Maybe FilePath) -- ^ Convert an ipkg file to yaml.
