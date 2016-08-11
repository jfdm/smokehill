-- | Because depending on Idris is too much.
module Smokehill.PackageDesc where

-- | Simple name space description
data Name = UName String
          | NSName String Name
          deriving (Show)

data PackageDesc = PackageDesc
  { pkgname       :: String
  , pkgdeps       :: [String]
  , pkgbrief      :: Maybe String
  , pkgversion    :: Maybe String
  , pkgreadme     :: Maybe String
  , pkglicense    :: Maybe String
  , pkgauthor     :: Maybe String
  , pkgmaintainer :: Maybe String
  , pkghomepage   :: Maybe String
  , pkgsourceloc  :: Maybe String
  , pkgbugtracker :: Maybe String
  , libdeps       :: [String]
  , objs          :: [String]
  , makefile      :: Maybe String
  , idris_opts    :: String
  , sourcedir     :: String
  , modules       :: [Name]
  , idris_main    :: Maybe Name
  , execout       :: Maybe String
  , idris_tests   :: [Name]
  }
  deriving (Show)

defaultPkg :: PackageDesc
defaultPkg = PackageDesc
  "" [] Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing
  Nothing [] [] Nothing [] "" [] Nothing Nothing []


toName :: [String] -> Maybe Name
toName []       = Nothing
toName (x:[])   = Just $ UName x
toName (x:y:xs) = case toName xs of
    Nothing -> Just $ NSName x (UName y)
    Just ns -> Just $ NSName x (NSName y ns)

toString :: Name -> String
toString (UName s) = s
toString (NSName s n) = concat [s,"."] ++ toString n