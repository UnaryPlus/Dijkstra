module Test where

import Dijkstra

testGraph1 :: Graph
testGraph1 = makeGraph
  [ ("A", "C", 5)
  , ("A", "B", 8)
  , ("B", "C", 3)
  , ("B", "D", 1)
  , ("C", "D", 2)
  , ("D", "E", 2)
  , ("C", "E", 9)
  ]

testGraph2 :: Graph
testGraph2 = makeGraph
  [ ("S", "A", 2)
  , ("A", "B", 8)
  , ("B", "C", 5)
  , ("B", "F", 8)
  , ("F", "G", 2)
  , ("C", "G", 6)
  , ("C", "E", 4)
  , ("C", "D", 3)
  , ("D", "E", 7)
  , ("E", "H", 9)
  , ("H", "I", 9)
  , ("G", "I", 8)
  , ("G", "H", 5)
  , ("C", "H", 5)
  ]

main :: IO ()
main = putStrLn $ showDistanceList (dijkstra "S" testGraph2)
