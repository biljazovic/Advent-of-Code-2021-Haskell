module Day25 (main25) where

import Util
import Data.Array ( (!), (//) )
import qualified Data.Array as Arr

stepInDir :: Char -> (V2 Int -> V2 Int -> V2 Int) -> CharMatrix -> CharMatrix
stepInDir ch move mat = mat // concat [ [ (p, '.'), (move' p, ch) ] | p <- toMove, canMove p ]
  where
    move' = move (snd (Arr.bounds mat))
    toMove = mat & Arr.assocs & filter ((== ch) . snd) & map fst
    canMove p = (mat ! move' p) == '.'

step :: CharMatrix -> CharMatrix
step = stepInDir '>' moveEast >>> stepInDir 'v' moveSouth
  where
    moveEast (V2 n m) (V2 x y) = V2 x ((y+1) `mod` (m+1))
    moveSouth (V2 n m) (V2 x y) = V2 ((x+1) `mod` (n+1)) y

solve = pred . length . iterateUntil (uncurry (==)) step

main25 :: IO ()
main25 = do
    input <- parseMatrix <$> readFile "res/input25"
    print $ solve input
