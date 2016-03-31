-----------------------------------------------------------------------------
-- |
-- Module      : Bibliography
-- Description : Functions for renaming, sorting, and cleaning up bibliography
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- This module removes duplicates in a bibliography database, sorts
-- citations such that cross-referenced ones come after the referrers,
-- and renames citations according to the given rule table.
-----------------------------------------------------------------------------
module Bibliography (bibliography) where

import Control.Arrow ((&&&))

import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree

import Reference


bibliography :: [(RefIdent, RefIdent)] -> [BibTeX] -> [BibTeX]
bibliography renames database =
  let databaseMap = Map.fromList $ map (bibIdent &&& id) database
      renamed = map (\(s, t) -> renameBib t $ databaseMap Map.! s) renames
      renamedMap = Map.fromList $ map (bibIdent &&& id) renamed
      allMap = Map.union renamedMap databaseMap
      all = Map.elems allMap
  in topsort $ reachable (map snd renames) all


topsort :: [BibTeX] -> [BibTeX]
topsort bib = map ((\(bib, _, _) -> bib) . vertexMap) sorted
  where
    (g, vertexMap, _) = Graph.graphFromEdges $ map bibToNode bib
    sorted = Graph.topSort g

reachable :: [RefIdent] -> [BibTeX] -> [BibTeX]
reachable roots bib = map ((\(bib, _, _) -> bib) . vertexMap) reachableVertecies
  where
    (g, vertexMap, key) = Graph.graphFromEdges $ map bibToNode bib
    rootsVertex = mapMaybe key roots
    reachableVertecies = concatMap Tree.flatten (Graph.dfs g rootsVertex)

bibToNode :: BibTeX -> (BibTeX, RefIdent, [RefIdent])
bibToNode bib = (bib, bibIdent bib, crossrefs bib)
