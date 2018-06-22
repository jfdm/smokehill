module Smokehill.Dependency
  (
    getInstallOrder
  , showDependencyGraph
  ) where

import Data.Graph
import Data.Maybe
import Data.List

import Smokehill.IPackage
import Smokehill.PackageConfig
import Smokehill.Model
import Smokehill.Utils

import Utils

buildLegend :: List (String, [String])
            -> Int
            -> List (String,Int)
            -> List (String,Int)
buildLegend []         c cs = []
buildLegend ((x,_):xs) c cs = (x,c) : buildLegend xs (c+1) cs


data DepGraph a = DepGraph
  { legend :: List (Int, a)
  , graph  :: Graph
  } deriving (Show)

buildGraph :: List (String, List String)
           -> DepGraph String
buildGraph xs = DepGraph legend' graph
  where
    legend :: [(String,Int)]
    legend = buildLegend xs 1 []

    legend' = map (\(a,b)->(b,a)) legend

    graph :: Graph
    graph = buildG (1, length legend) (catMaybes $ map mergeMaybes buildEdgesStr)

    mergeMaybes :: (Maybe a, Maybe a) -> Maybe (a,a)
    mergeMaybes (Just x, Just y) = Just (x,y)
    mergeMaybes _                = Nothing


    buildEdge :: (String, List String) -> List (String,String)
    buildEdge (k,vs) = map (\v -> (k,v)) vs

    buildEdges :: List (String, List String) -> List (String,String)
    buildEdges = concatMap (\e -> buildEdge e)

    buildEdgesStr :: List (Maybe Int, Maybe Int)
    buildEdgesStr = map (\(a,b) -> (lookup a legend, lookup b legend)) (buildEdges xs)

pruneDeps :: List String -> List String
pruneDeps xs = (\\) xs ["base", "pruvoilj", "effects", "prelude", "contrib"]

getInfo :: Bool -> PackageConfig -> Smokehill (List (String, List String))
getInfo prune ipkg = doGet [ipkg] []
  where
    doGet :: List PackageConfig
          -> List (String, List String)
          -> Smokehill (List (String, List String))
    doGet []     res = return res
    doGet (x:xs) res = do
      case pruneDeps (deps x) of
        [] -> doGet xs ([(name x, [])] ++ res)
        pdeps -> do
          ds' <- mapM searchPackages pdeps
          let ds = catMaybes ds'
          let nodes = if prune then pdeps else deps x
          doGet (ds ++ xs) ([(name x, nodes)] ++ res)

sBuildGraph :: Bool -> PackageConfig -> Smokehill (DepGraph String)
sBuildGraph p ipkg = do
  ds <- getInfo p ipkg
  pure (buildGraph ds)

showDependencyGraph :: PackageConfig -> Smokehill ()
showDependencyGraph ipkg = do
  g <- sBuildGraph False ipkg
  sPrintLn g

getInstallOrder :: PackageConfig -> Smokehill (List PackageConfig)
getInstallOrder ipkg = do
    dg <- sBuildGraph True ipkg
    let os  = reverse $ topSort (graph dg)
        os' = map (\o -> lookup o (legend dg)) os
        ds' = nub $ catMaybes os'
    res <- mapM searchPackages ds'
    pure $ catMaybes res
