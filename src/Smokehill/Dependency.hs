module Smokehill.Dependency
  (
    getInstallOrder
  ) where

import Data.Graph
import Data.Maybe
import Data.List

import Idris.Package.Common
import Idris.Package

import Smokehill.URL
import Smokehill.Utils

import Utils

buildLegend :: [(String, [String])] -> Int -> [(String,Int)] -> [(String,Int)]
buildLegend []         c cs = []
buildLegend ((x,_):xs) c cs = (x,c) : buildLegend xs (c+1) cs


data DepGraph a = DepGraph
  { legend :: [(Int, a)]
  , graph  :: Graph
  } deriving (Show)

buildGraph :: [(String, [String])] -> DepGraph String
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


    buildEdge :: (String, [String]) -> [(String,String)]
    buildEdge (k,vs) = map (\v -> (k,v)) vs

    buildEdges :: [(String, [String])] -> [(String,String)]
    buildEdges = concatMap (\e -> buildEdge e)

    buildEdgesStr :: [(Maybe Int, Maybe Int)]
    buildEdgesStr = map (\(a,b) -> (lookup a legend, lookup b legend)) (buildEdges xs)

pruneDeps :: [String] -> [String]
pruneDeps xs = (\\) xs ["base", "pruvoilj", "effects", "prelude"]

getInstallOrder :: PkgDesc -> IO (List FilePath)
getInstallOrder ipkg = do
    ds <- doGet [ipkg] []
    putStrLn $ show ds
    let dg  = buildGraph ds
        os  = reverse $ topSort (graph dg)
        os' = map (\o -> lookup o (legend dg)) os
    pure $ nub $ catMaybes os'
  where
    doGet :: List PkgDesc
          -> List (String, List String)
          -> IO (List (String, List String))
    doGet []     res = return res
    doGet (x:xs) res = do
      let deps = pruneDeps (pkgdeps x)
      ds <- mapM getPkgFile deps
      doGet (ds ++ xs) ([(pkgname x, deps)] ++ res)
