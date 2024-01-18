module DominatorTree (DomTree, domTreeTree, domTreeMapping, generateDominatorTree) where

import qualified Data.Map
import Data.Graph
import Data.Graph.Dom
import Data.IntMap
import Data.IntSet

-- a is used to represent the node type
-- b is used to represent the label(id) type

type GetEdges a b = (a -> [b])

type GetId a b = (a -> b)

data DomTree b = DomTree {
  domTreeTree :: Tree Int,
  domTreeMapping :: IntMap b
}

-- Assumes that the first node is the entry node
generateDominatorTree :: Ord b => [a] -> GetId a b -> GetEdges a b -> DomTree b
generateDominatorTree nodes getId getEdges = DomTree {domTreeTree = tree, domTreeMapping = mapping}
  where
    (edges, mapping) = listToGraph nodes getId getEdges
    graph = (0, edges)
    tree = domTree graph

listToGraph :: Ord b => [a] -> GetId a b -> GetEdges a b -> (IntMap IntSet, IntMap b)
listToGraph nodes getId getEdges = (edges, mapping)
  where
    (mapping, reverseMapping) = mapIds nodes 0 getId
    edges = listToGraphAux nodes reverseMapping getId getEdges

listToGraphAux :: Ord b => [a] -> Data.Map.Map b Int -> GetId a b -> GetEdges a b -> IntMap IntSet
listToGraphAux [] _ _ _ = Data.IntMap.empty
listToGraphAux (node:tail) mapping getId getEdges = edges'
  where
    edges = listToGraphAux tail mapping getId getEdges
    nodeOriginalId = getId node
    nodeOriginalEdges = getEdges node
    nodeId = mapping Data.Map.! nodeOriginalId
    nodeEdgesList = Prelude.map (mapping Data.Map.!) nodeOriginalEdges
    nodeEdges = Data.IntSet.fromList nodeEdgesList
    edges' = Data.IntMap.insert nodeId nodeEdges edges

mapIds :: Ord b => [a] -> Int -> GetId a b -> (IntMap b, Data.Map.Map b Int)
mapIds [] _ _ = (Data.IntMap.empty, Data.Map.empty)
mapIds (node:tail) nextId getId = (mapping', reverseMapping')
  where
    (mapping, reverseMapping) = mapIds tail (nextId + 1) getId
    nodeOriginalId = getId node
    mapping' = Data.IntMap.insert nextId nodeOriginalId mapping
    reverseMapping' = Data.Map.insert nodeOriginalId nextId reverseMapping
