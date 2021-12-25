module Day12 (main12) where

import Util
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isUpper)

type Node = String
type Graph = Map Node [Node]

mkGraph :: [[String]] -> Graph
mkGraph = concatMap (\(a:b:_) -> [(a, [b]), (b, [a])]) >>> Map.fromListWith (++)

isBig :: String -> Bool
isBig = all isUpper

isSmall = not . isBig

endNode = "end"
startNode = "start"

solveA :: Graph -> Int
solveA graph = go (Set.singleton startNode) startNode
  where
    go :: Set Node -> Node -> Int
    go visited node = if node == endNode 
                               then 1
                               else sum $ map f (graph Map.! node)
      where
        f neigh = if isSmall neigh && neigh `Set.member` visited
                     then 0
                     else go (Set.insert neigh visited) neigh 

solveB :: Graph -> Int
solveB graph = go False (Set.singleton startNode) startNode
  where
    go :: Bool -> Set Node -> Node -> Int
    go visitedTwice visited node = if node == endNode 
                               then 1
                               else sum $ map f (graph Map.! node)
      where
        f neigh = let newVisited = Set.insert neigh visited
                   in if isSmall neigh && neigh `Set.member` visited
                         then if visitedTwice || neigh == startNode
                                 then 0
                                 else go True newVisited neigh
                         else go visitedTwice newVisited neigh 

main12 :: IO ()
main12 = do
    input <- mkGraph . map (splitOn "-") . lines <$> readFile "res/input12"
    print $ solveA input
    print $ solveB input
