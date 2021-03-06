module Smokehill.Model where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Class as Trans (lift)

import System.Exit
import System.IO
import System.IO.Error(isUserError, ioeGetErrorString, tryIOError)

import Smokehill.IPackage
import Smokehill.DVCS
import Smokehill.PackageConfig

import Utils

--  ------------------------------------------------ [ Smokehill Program Model ]

type Smokehill = StateT SState (ExceptT SError IO)

-- | Program launch point
runMain :: Smokehill () -> IO ()
runMain prog = do
  res <- runExceptT $ execStateT prog initState
  case res of
    Left err -> do
      putStrLn "Uncaught error: "
      print err
    Right _ -> return ()

--  -------------------------------------------------------------- [ State API ]

data SState = SState
  {
    library :: [PackageConfig]
  , iexe    :: FilePath
  , dbloc   :: DVCS
  } deriving (Show)

initState :: SState
initState = SState [] "" (newGitRepo "https://www.github.com/jfdm/chipshop.git")

getState :: Smokehill SState
getState = get

putState :: SState -> Smokehill ()
putState = put

updateState :: (SState -> SState) -> Smokehill ()
updateState u = do
  s <- getState
  putState (u s)


--  ---------------------------------------------------------------- [ Lib API ]

setLibrary :: [PackageConfig] -> Smokehill ()
setLibrary ls = do
  st <- getState
  put (st {library = ls})

getLibrary :: Smokehill [PackageConfig]
getLibrary = do
  st <- getState
  return (library st)

setIdrisExe :: FilePath -> Smokehill ()
setIdrisExe fp = do
  st <- getState
  put (st {iexe = fp})

getIdrisExe :: Smokehill FilePath
getIdrisExe = do
  st <- getState
  return (iexe st)

setPackageRepo :: DVCS -> Smokehill ()
setPackageRepo fp = do
  st <- getState
  put (st {dbloc = fp})

getPackageRepo :: Smokehill DVCS
getPackageRepo = do
  st <- getState
  return (dbloc st)

--  -------------------------------------------------------------- [ Error API ]

data SError = DVCSError    String
            | LibraryError String
            | PackageError String
            | RepoError    String
            | ConfigParserError String
            | ConvertError String
            | IOErr        IOError
            | NotImplemented String
            deriving (Show)

catchError :: Smokehill a -> (SError -> Smokehill a) -> Smokehill a
catchError = liftCatch catchE

throwError :: SError -> Smokehill a
throwError = Trans.lift . throwE

failIO :: String -> IO a
failIO s = do
  putStrLn s
  exitFailure

die :: Smokehill () -> Smokehill a
die a = do
  a
  runIO exitFailure

die' :: IO () -> Smokehill a
die' a = do
  runIO a
  runIO exitFailure

dieIO :: IO () -> IO a
dieIO a = do
  a
  exitFailure

--  --------------------------------------------------------- [ IO Adjustments ]

runIO :: IO a -> Smokehill a
runIO x = liftIO (tryIOError x) >>= either (throwError . IOErr) return

printLn :: Show a => a -> IO ()
printLn = putStrLn . show

sPrintLn :: Show a => a -> Smokehill ()
sPrintLn = runIO . putStrLn . show

sPutStrLn :: String -> Smokehill ()
sPutStrLn = runIO . putStrLn

sPutWordsLn :: [String] -> Smokehill ()
sPutWordsLn ws = sPutStrLn (unwords ws)

--  -------------------------------------------------------------------- [ EOF ]
