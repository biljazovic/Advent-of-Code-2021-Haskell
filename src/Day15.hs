module Day15 (main15) where

import Util hiding (generateGraph)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Array as Arr
import Data.Graph.Inductive.Graph ( Graph(mkGraph) )
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.Char (digitToInt)
import Data.Graph.Inductive.Query (spLength)

generateGraph :: Arr.Array (V2 Int) Int -> (Gr Int Int, Map (V2 Int) Int)
generateGraph mat = (mkGraph nodes edges, indexMap)
  where
    coords = Arr.indices mat
    indexMap = Map.fromList $ zip coords [1 ..]
    nodes = zipWith (\coord index -> (index, mat Arr.! coord)) coords [1 ..]
    edges = concatMap f coords
    f coord = map g (susedi4 (Just $ Arr.bounds mat) coord)
      where
        g sused = (indexMap Map.! coord, indexMap Map.! sused, mat Arr.! sused)

solve :: Arr.Array (V2 Int) Int -> Int
solve mat = fromJust $ spLength startNode endNode graph
  where
    (graph, indexMap) = generateGraph mat
    startNode = indexMap Map.! fst (Arr.bounds mat)
    endNode = indexMap Map.! snd (Arr.bounds mat)

solveA = solve

solveB mat = solve newMat
  where
    newMat = Arr.array (V2 0 0, V2 (5*n-1) (5*m-1)) $ [ (V2 i j, f (V2 i j)) | i <- [0..5*n-1] , j <- [0..5*m-1] ]
    f (V2 i j) = let x = mat Arr.! V2 (i `mod` n) (j `mod` m)
                     plus1 y = if y == 9 then 1 else y+1
                     plus y1 y2 = iterate plus1 y1 !! y2
                  in plus (plus x (i `div` n)) (j `div` m)
    (_, V2 ((+1) -> n) ((+1) -> m)) = Arr.bounds mat
    

main15 :: IO ()
main15 = do
    input <- fmap digitToInt . parseMatrix <$> readFile "res/input15"
    print $ solveA input
    print $ solveB input
