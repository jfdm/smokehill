module Smokehill.IPackage.Parser
  (
    parsePkgDesc
  , parsePkgDescFile
  , parsePkgDescFileIO
  ) where

import System.Exit
import Control.Monad (void)
import Control.Monad.State.Lazy
import Control.Monad.Identity

import Control.Monad.Writer.Strict (MonadWriter(..), WriterT(..), listen, runWriterT, tell)

import Data.Either
import Data.List (union)
import Data.Void (Void(..))

import Text.Megaparsec

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Smokehill.Model
import Smokehill.IPackage

--  ------------------------------------------------------------ [ Definitions ]

commentLn   = "--"
commentBlkL = "{--"
commentBlkR = "--}"

reservedWords = []

--  ----------------------------------------------------------------- [ Parser ]


type IPkgParser = StateT IPackage (P.Parsec Void String)

parsePkgDesc :: FilePath -> String -> Either String IPackage
parsePkgDesc fn str =
  case P.parse (evalStateT pkgDesc defaultPkg) fn str of
     Left err  -> Left $ parseErrorPretty err
     Right res -> Right res

parsePkgDescFile :: FilePath -> Smokehill (IPackage)
parsePkgDescFile fn = do
  str <- runIO $ readFile fn

  case parsePkgDesc fn str of
    Left err -> die' $ do
      putStrLn "Uncaught error: "
      putStr err
      exitFailure
    Right x -> pure x

parsePkgDescFileIO :: FilePath -> IO (IPackage)
parsePkgDescFileIO fn = do
  str <- readFile fn

  case parsePkgDesc fn str of
    Left err -> do
      putStrLn "Uncaught error: "
      putStr err
      exitFailure
    Right x -> return x

pkgDesc :: IPkgParser IPackage
pkgDesc = do
    st <- get
    keyword "package"
    pname <- identifier
    put (st {pkgname = pname})
    some kvpairs
    eof
    st <- get
    return st
  <?> "PkgDesc"

kvpairs :: IPkgParser ()
kvpairs = executable
      <|> srcdir
      <|> opts
      <|> pkgs
      <|> imodules
      <|> libs
      <|> iobjs
      <|> imakefile
      <|> tests
      <|> version
      <|> readme
      <|> license
      <|> sourceloc
      <|> bugtracker
      <|> brief
      <|> author
      <|> maintainer
      <|> main
      <|> homepage
      <?> "Key Value Pairs"

executable :: IPkgParser ()
executable = do
  keyword "executable"
  equals
  en <- identifier
  st <- get
  put (st {execout = Just en})

main :: IPkgParser ()
main = do
  keyword "main"
  equals
  mn <- name
  st <- get
  put (st {idris_main = Just mn})

srcdir :: IPkgParser ()
srcdir = do
  keyword "sourcedir"
  equals
  sdir <- identifier
  st <- get
  put (st {sourcedir = sdir})

opts :: IPkgParser ()
opts = do
  keyword "opts"
  equals
  os <- stringLiteral
  P.eol
  sc
  st <- get
  put (st {idris_opts = unwords [os, idris_opts st]})

pkgs :: IPkgParser ()
pkgs = do
  keyword "pkgs"
  equals
  ps <- sepBy1 identifier comma
  st <- get
  put (st { pkgdeps = ps `union` (pkgdeps st)})

imodules :: IPkgParser ()
imodules = do
  keyword "modules"
  equals
  ms <- sepBy1 name comma
  st <- get
  put (st {modules = modules st ++ ms})

libs :: IPkgParser ()
libs = do
  keyword "libs"
  equals
  ms <- sepBy1 name comma
  st <- get
  put (st {libdeps = map toString ms ++ libdeps st})

iobjs :: IPkgParser ()
iobjs = do
  keyword "objs"
  equals
  ms <- sepBy1 name comma
  st <- get
  put (st {objs = map toString ms ++ objs st})

imakefile :: IPkgParser ()
imakefile = do
  keyword "makefile"
  equals
  mf <- name
  st <- get
  put (st {makefile = Just (toString mf)})


tests :: IPkgParser ()
tests = do
  keyword "tests"
  equals
  ts <- sepBy1 name comma
  st <- get
  put (st {idris_tests = idris_tests st ++ ts})

version :: IPkgParser ()
version = do
  keyword "version"
  equals
  x <- manyTill P.anyChar P.eol
  sc
  st <- get
  put (st {pkgversion = Just x})

readme :: IPkgParser ()
readme = do
  keyword "readme"
  equals
  x <- manyTill P.anyChar P.eol
  sc
  st <- get
  put (st {pkgreadme = Just x})

license :: IPkgParser ()
license = do
  keyword "license"
  equals
  x <- manyTill P.anyChar P.eol
  sc
  st <- get
  put (st {pkglicense = Just x})

sourceloc :: IPkgParser ()
sourceloc = do
  keyword "sourceloc"
  equals
  x <- manyTill P.anyChar P.eol
  sc
  st <- get
  put (st {pkgsourceloc = Just x})


bugtracker :: IPkgParser ()
bugtracker = do
  keyword "bugtracker"
  equals
  x <- manyTill P.anyChar P.eol
  sc
  st <- get
  put (st {pkgbugtracker = Just x})

brief :: IPkgParser ()
brief = do
  keyword "brief"
  equals
  x <- stringLiteral
  P.eol
  sc
  st <- get
  put (st {pkgbrief = Just x})

author :: IPkgParser ()
author = do
  keyword "author"
  equals
  x <- manyTill P.anyChar P.eol
  sc
  st <- get
  put (st {pkgauthor = Just x})

maintainer :: IPkgParser ()
maintainer = do
  keyword "maintainer"
  equals
  x <- manyTill P.anyChar P.eol
  sc
  st <- get
  put (st {pkgmaintainer = Just x})

homepage :: IPkgParser ()
homepage = do
  keyword "homepage"
  equals
  x <- manyTill P.anyChar P.eol
  sc
  st <- get
  put (st {pkghomepage = Just x})

--  ------------------------------------------------------------------ [ Idris ]

name :: IPkgParser Name
name = do
    ns <- sepBy1 identifier period
    case toName ns of
      Nothing -> fail "oops"
      Just n  -> return n

--  ------------------------------------------------------------------ [ Lexer ]
lexeme :: IPkgParser a -> IPkgParser a
lexeme = L.lexeme sc

symbol :: String -> IPkgParser String
symbol = L.symbol sc

equals :: IPkgParser ()
equals = do
  symbol "="
  return ()

comma :: IPkgParser ()
comma = do
  symbol ","
  return ()

period :: IPkgParser ()
period = do
  symbol "."
  return ()

keyword :: String -> IPkgParser String
keyword w = do
  s <- P.string w
  notFollowedBy P.alphaNumChar
  sc
  return s

stringLiteral :: IPkgParser String
stringLiteral = P.char '"' >> manyTill L.charLiteral (P.char '"')

identifier :: IPkgParser String
identifier = lexeme (p >>= check)
  where
    p       = (:) <$> P.letterChar <*> many P.alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

sc :: IPkgParser ()
sc = L.space (void P.spaceChar) lineCmt blockCmt
  where
    lineCmt = L.skipLineComment commentLn
    blockCmt = L.skipBlockComment commentBlkL commentBlkR
--  -------------------------------------------------------------------- [ EOF ]
