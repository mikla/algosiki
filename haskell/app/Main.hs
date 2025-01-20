module Main (main) where

import Data.Graph.Inductive

myGraph :: Gr String String
myGraph = mkGraph nodes edges
  where
    nodes =
      [ (1, "a"),
        (2, "b"),
        (3, "c")
      ]
    edges =
      [ (1, 2, "right"),
        (2, 3, "down"),
        (3, 1, "up"),
        (2, 1, "left")
      ]

main :: IO ()
main = do
  -- Run DFS starting from node 1
  let dfsNodes = dfs [3] myGraph
  putStrLn "DFS traversal order:"
  print dfsNodes

  case match 2 myGraph of
    (Nothing, _) -> putStrLn "Node not found"
    (Just context, remainingGraph) -> do
      putStrLn "Found node context:"
      print context
      putStrLn "\nRemaining graph:"
      print remainingGraph
