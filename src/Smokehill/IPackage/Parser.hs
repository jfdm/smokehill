module Smokehill.IPackage.Parser
  (
    parsePkgDesc
  , parsePkgDescFile
  ) where

import System.Exit
import Control.Monad (void)
import Control.Monad.State.Lazy
import Control.Monad.Identity

import Data.Either
import Data.List (union)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String -- input stream is of type ‘String’
import Text.Megaparsec.Error

import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Prim  as P (runParser)

import Smokehill.Model
import Smokehill.IPackage

--  ------------------------------------------------------------ [ Definitions ]

commentLn   = "--"
commentBlkL = "{--"
commentBlkR = "--}"

reservedWords = []

--  ----------------------------------------------------------------- [ Parser ]

type PParser a = StateT IPackage Parser a

parsePkgDesc :: FilePath -> String -> Either String IPackage
parsePkgDesc fn str =
  either (\x -> Left $ parseErrorPretty x)
         Right
         (P.runParser (execStateT pkgDesc defaultPkg) fn str)


parsePkgDescFile :: FilePath -> IO (IPackage)
parsePkgDescFile fn = do
  str <- readFile fn

  case parsePkgDesc fn str of
    Left err -> do
      putStrLn "Uncaught error: "
      putStr err
      exitFailure
    Right x -> return x

pkgDesc :: PParser IPackage
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

kvpairs :: PParser ()
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

executable :: PParser ()
executable = do
  keyword "executable"
  equals
  en <- identifier
  st <- get
  put (st {execout = Just en})

main :: PParser ()
main = do
  keyword "main"
  equals
  mn <- name
  st <- get
  put (st {idris_main = Just mn})

srcdir :: PParser ()
srcdir = do
  keyword "sourcedir"
  equals
  sdir <- identifier
  st <- get
  put (st {sourcedir = sdir})

opts :: PParser ()
opts = do
  keyword "opts"
  equals
  os <- stringLiteral
  eol
  sc
  st <- get
  put (st {idris_opts = unwords [os, idris_opts st]})

pkgs :: PParser ()
pkgs = do
  keyword "pkgs"
  equals
  ps <- sepBy1 identifier comma
  st <- get
  put (st { pkgdeps = ps `union` (pkgdeps st)})

imodules :: PParser ()
imodules = do
  keyword "modules"
  equals
  ms <- sepBy1 name comma
  st <- get
  put (st {modules = modules st ++ ms})

libs :: PParser ()
libs = do
  keyword "libs"
  equals
  ms <- sepBy1 name comma
  st <- get
  put (st {libdeps = map toString ms ++ libdeps st})

iobjs :: PParser ()
iobjs = do
  keyword "objs"
  equals
  ms <- sepBy1 name comma
  st <- get
  put (st {objs = map toString ms ++ objs st})

imakefile :: PParser ()
imakefile = do
  keyword "makefile"
  equals
  mf <- name
  st <- get
  put (st {makefile = Just (toString mf)})


tests :: PParser ()
tests = do
  keyword "tests"
  equals
  ts <- sepBy1 name comma
  st <- get
  put (st {idris_tests = idris_tests st ++ ts})

version :: PParser ()
version = do
  keyword "version"
  equals
  x <- manyTill anyChar eol
  sc
  st <- get
  put (st {pkgversion = Just x})

readme :: PParser ()
readme = do
  keyword "readme"
  equals
  x <- manyTill anyChar eol
  sc
  st <- get
  put (st {pkgreadme = Just x})

license :: PParser ()
license = do
  keyword "license"
  equals
  x <- manyTill anyChar eol
  sc
  st <- get
  put (st {pkglicense = Just x})

sourceloc :: PParser ()
sourceloc = do
  keyword "sourceloc"
  equals
  x <- manyTill anyChar eol
  sc
  st <- get
  put (st {pkgsourceloc = Just x})


bugtracker :: PParser ()
bugtracker = do
  keyword "bugtracker"
  equals
  x <- manyTill anyChar eol
  sc
  st <- get
  put (st {pkgbugtracker = Just x})

brief :: PParser ()
brief = do
  keyword "brief"
  equals
  x <- stringLiteral
  eol
  sc
  st <- get
  put (st {pkgbrief = Just x})

author :: PParser ()
author = do
  keyword "author"
  equals
  x <- manyTill anyChar eol
  sc
  st <- get
  put (st {pkgauthor = Just x})

maintainer :: PParser ()
maintainer = do
  keyword "maintainer"
  equals
  x <- manyTill anyChar eol
  sc
  st <- get
  put (st {pkgmaintainer = Just x})

homepage :: PParser ()
homepage = do
  keyword "homepage"
  equals
  x <- manyTill anyChar eol
  sc
  st <- get
  put (st {pkghomepage = Just x})

--  ------------------------------------------------------------------ [ Idris ]

name :: PParser Name
name = do
    ns <- sepBy1 identifier period
    case toName ns of
      Nothing -> fail "oops"
      Just n  -> return n

--  ------------------------------------------------------------------ [ Lexer ]
lexeme :: PParser a -> PParser a
lexeme = L.lexeme sc

symbol :: String -> PParser String
symbol = L.symbol sc

equals :: PParser ()
equals = do
  symbol "="
  return ()

comma :: PParser ()
comma = do
  symbol ","
  return ()

period :: PParser ()
period = do
  symbol "."
  return ()

keyword :: String -> PParser String
keyword w = do
  s <- string w
  notFollowedBy alphaNumChar
  sc
  return s

stringLiteral :: PParser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

identifier :: PParser String
identifier = lexeme (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

sc :: PParser ()
sc = L.space (void spaceChar) lineCmt blockCmt
  where
    lineCmt = L.skipLineComment commentLn
    blockCmt = L.skipBlockComment commentBlkL commentBlkR
--  -------------------------------------------------------------------- [ EOF ]
