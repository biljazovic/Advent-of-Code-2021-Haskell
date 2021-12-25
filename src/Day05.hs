module Day05 (main05) where

import Util
import Text.Scanf
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

type Segment = (V2 Int, V2 Int)
type IT = [Segment]

parse :: String -> Maybe Segment
parse str = do
  (x :+ (y :+ (z :+ (w :+ ())))) <- scanf [fmt|%d,%d -> %d,%d|] str
  return (V2 x y, V2 z w)

solve :: IT -> Int
solve segs = rez
  where
    (_, Set.size -> rez) = foldr add (Set.empty, Set.empty) segs
    add seg (added, overlaps) = let bees = between seg
                                    overs = filter (`Set.member` added) bees
                                 in (foldr Set.insert added bees, foldr Set.insert overlaps overs)

solveA :: IT -> Int
solveA = solve . filter (\(V2 x1 y1, V2 x2 y2) -> x1 == x2 || y1 == y2)

solveB :: IT -> Int
solveB = solve

between :: Segment -> [V2 Int]
between (V2 x1 y1, V2 x2 y2)
  | x1 == x2 = [ V2 x1 y | y <- ys ]
  | y1 == y2 = [ V2 x y1 | x <- xs ]
  | otherwise = zipWith V2 xs ys
    where
      xs = from x1 x2
      ys = from y1 y2
      from z1 z2 = let zs = [ min z1 z2 .. max z1 z2 ]
                    in if z1 <= z2
                          then zs
                          else reverse zs

main05 :: IO ()
main05 = do
    input <- mapMaybe parse . filter (not . emptyLine) . lines <$> readFile "res/input05"
    print $ solveA input
    print $ solveB input
