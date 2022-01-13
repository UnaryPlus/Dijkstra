{-# LANGUAGE BlockArguments #-}

module Dijkstra
  ( Node
  , Distance
  , Graph
  , makeGraph
  , DistanceList
  , showDistanceList
  , dijkstra
  )
  where

import qualified Data.Maybe as Maybe
import Data.Function ((&))

import qualified Data.Map as Map
import Data.Map (Map, (!))

--graph nodes are labeled by strings
type Node = String

--cost of traveling between two nodes
data Distance = Finite Int | Infinite
  deriving (Eq, Ord)

--add two distances (anything plus infinity is infinity)
instance Semigroup Distance where
  Finite x <> Finite y = Finite (x + y)
  _ <> _ = Infinite

--reference to previous node (start node has none)
data Previous = After Node | None

--each node in graph has a list of adjacent nodes
type Graph = Map Node [(Distance, Node)]

--add a bidirectional connection (edge) to a graph
addConnection :: (Node, Node, Int) -> Graph -> Graph
addConnection (node1, node2, dist) = let
  add node cons = Just $ (Finite dist, node) : Maybe.fromMaybe [] cons
  in Map.alter (add node2) node1
  . Map.alter (add node1) node2

--make a graph from a list of connections
makeGraph :: [(Node, Node, Int)] -> Graph
makeGraph = foldr addConnection Map.empty

--result of dijkstra's algorithm
--each node has a distance from start and a reference to previous node
type DistanceList = Map Node (Distance, Previous)

--two lists: unvisited nodes and visited nodes
--by the end, the unvisited list will be empty
data DijkstraState = DijkstraState
  { getUnvisited :: DistanceList
  , getVisited :: DistanceList
  }

--at the beginning, all distances except the start node are set to infinity
initialList :: Node -> Graph -> DistanceList
initialList startNode = Map.mapWithKey \node _ ->
  (if node == startNode then Finite 0 else Infinite, None)

--run dijkstra's algorithm, given a start node and a graph
--1. repeat 'dijkstraStep' until the unvisited list is empty
--2. return the visited list
dijkstra :: Node -> Graph -> DistanceList
dijkstra startNode graph = let
  unvisited = initialList startNode graph
  visited = Map.empty
  in DijkstraState unvisited visited
    & until (Map.null . getUnvisited) (dijkstraStep graph)
    & getVisited

--look at the nodes that haven't been visited
--which one is closest to the start?
minDistance :: DistanceList -> (Distance, Node)
minDistance = let
  findMin node (dist, _) (minDist, minNode) =
    if dist <= minDist then (dist, node) else (minDist, minNode)
  in Map.foldrWithKey findMin (Infinite, "")

--given a current node and an adjacent node,
--add distance from start to current node to distance between nodes
--if result is less than than distance already known,
--update the list of distances
checkConnection
  :: DistanceList
  -> (Distance, Node)
  -> (Distance, Node)
  -> Maybe (Node, Distance, Previous)
checkConnection unvisited (minDist, minNode) (dist, node) =
  case Map.lookup node unvisited of
    Nothing -> Nothing
    Just (oldDist, _) ->
      if minDist <> dist < oldDist
        then Just (node, minDist <> dist, After minNode)
        else Nothing

--apply a list of updates to a distance list
updateList :: [(Node, Distance, Previous)] -> DistanceList -> DistanceList
updateList = flip $ foldr
  \(node, dist, prev) -> Map.insert node (dist, prev)

--move a key from the first map to the second map
moveKey :: (Ord k) => k -> Map k a -> Map k a -> Map k a
moveKey key map1 map2 =
  case Map.lookup key map1 of
    Nothing -> map2
    Just value -> Map.insert key value map2

--run one step of dijkstra's algorithm
--1. find the node closest to start
--2. check all of its connections
--3. move it to the visited list
dijkstraStep :: Graph -> DijkstraState -> DijkstraState
dijkstraStep graph (DijkstraState unvisited visited) = let
  (minDist, minNode) = minDistance unvisited
  connections = graph ! minNode
  updates = Maybe.mapMaybe (checkConnection unvisited (minDist, minNode)) connections
  unvisited' = Map.delete minNode (updateList updates unvisited)
  in DijkstraState unvisited' (moveKey minNode unvisited visited)

showDistance :: Distance -> String
showDistance (Finite x) = show x
showDistance Infinite = "infinity"

showPrevious :: Previous -> String
showPrevious (After x) = "after " ++ x
showPrevious None = "start"

showDistanceList :: DistanceList -> String
showDistanceList = let
  showEntry (node, (dist, prev)) = concat
    [ node, ": ", showDistance dist, " (", showPrevious prev, ")\n" ]
  in concatMap showEntry . Map.toList
