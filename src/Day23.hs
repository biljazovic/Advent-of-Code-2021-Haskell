module Day23 (main23) where

import Util
import Data.Maybe (maybeToList)
import Data.Array ( (!), Array )
import qualified Data.Array as Arr
import Control.Monad (guard)
import Data.Hashable (Hashable (hashWithSalt), hashUsing)
import Data.String.QQ

type Point = V2 Int

emptySusedi :: CharMatrix -> Point -> [Point]
emptySusedi mat p = susedi4 (Just (Arr.bounds mat)) p
                    & filter ((== '.') . (mat !))

distFromTo :: CharMatrix -> Point -> Point -> Maybe Int
distFromTo mat start end = fmap (* point) dist
  where
    dist = genericBfs (== end) (emptySusedi mat) start
    point = maybe 0 (points !!) (elemIndex (mat ! start) svi)

points = [ 1, 10, 100, 1000 ]
svi = "ABCD"

homeIndices :: CharMatrix -> Int -> [Point]
homeIndices mat ind = filter ((/= '#') . (mat !)) [ V2 x (3+ind*2) | x <- [2..n] ]
  where
    (_, V2 n m) = Arr.bounds mat

findHome :: CharMatrix -> Point -> Maybe Point
findHome mat p = do let tko = mat ! p
                    tkoInd <- elemIndex tko svi
                    let kands = reverse $ homeIndices mat tkoInd
                        emptyKands = dropWhile ((== tko) . (mat !)) kands
                    guard $ not (null emptyKands) && all ((== '.') . (mat !)) emptyKands
                    Just $ head emptyKands

hallwayX = 1

hallwayIndices :: CharMatrix -> [Point]
hallwayIndices mat = filter upOfWall $ filter ((== '.') . (mat !)) [ V2 hallwayX y | y <- [0..m] ]
  where
    (_, V2 n m) = Arr.bounds mat
    upOfWall (V2 x y) = mat ! V2 (x+1) y == '#'

matrixSusedi :: CharMatrix -> [(CharMatrix, Int)]
matrixSusedi mat = do p@(V2 x y) <- toMove
                      if x == hallwayX
                         then do
                           p' <- maybeToList (findHome mat p)
                           f p p'
                         else concatMap (f p) (hallwayIndices mat)
  where
    toMove = Arr.assocs mat & filter (snd >>> (`elem` svi)) & map fst
    move p p' = mat Arr.// [ (p, '.'), (p', mat!p) ]
    f p p' = do
      dist <- maybeToList $ distFromTo mat p p'
      [ (move p p', dist) ]

isGoal :: CharMatrix -> Bool
isGoal mat = all isGoalForInd [0..3]
  where
    isGoalForInd ind = all ((== (svi !! ind)) . (mat !)) $ homeIndices mat ind

instance Hashable (Array (V2 Int) Char) where
  hashWithSalt = hashUsing Arr.assocs

solve :: CharMatrix -> Maybe Int
solve mat = dijkstra mat isGoal matrixSusedi

expandString = [s|
  #D#C#B#A#
  #D#B#A#C#
|]

main23 :: IO ()
main23 = do
  input <- readFile "res/input23"
  let strs = filter (not . emptyLine) $ lines input
      inputA = parseMatrix $ unlines strs
      inputB = parseMatrix $ unlines (take 3 strs ++ lines expandString ++ drop 3 strs)
  print $ solve inputA
  print $ solve inputB
