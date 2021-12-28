module Day17 (main17) where

import Util
import Text.Scanf
import Control.Monad (guard)
import Data.Ix (inRange)

parse str = do
  (x1 :+ x2 :+ y1 :+ y2 :+ ()) <- scanf [fmt|target area: x=%d..%d, y=%d..%d|] str
  guard (y2 < 0)
  return ((x1, x2), (y1, y2))

solveA (_, (y1, _)) = let y = abs y1
                        in y*(y-1) `div` 2

solveB :: ((Int, Int), (Int, Int)) -> Int
solveB ((x1, x2), (y1, y2)) = listCount good [ (x', y') | x' <- [-x..x], y' <- [-y..y] ]
  where
    y = abs y1
    x = max (abs x1) (abs x2)
    good (vx, vy) = any (inArea . fst) $ iterateUntil (belowArea . fst . snd) step ((0, 0), (vx, vy))
    inArea (x', y') = inRange (x1, x2) x' && inRange (y1, y2) y'
    belowArea (_, y') = y' < y1
    step ((x', y'), (vx', vy')) = 
      let vx'' = if | vx' == 0 -> 0
                    | vx' < 0 -> vx'+1
                    | otherwise -> vx'-1
       in ((x'+vx', y'+vy'), (vx'', vy'-1))

-- $> main17
main17 :: IO ()
main17 = do
    Just input <- parse . head . lines <$> readFile "res/input17"
    print $ solveA input
    print $ solveB input
