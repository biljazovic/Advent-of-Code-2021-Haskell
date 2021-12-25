module Day11 (main11) where

import qualified Data.Array as Arr
import Util

succ' :: Char -> Char
succ' c
  | c == 'G' = 'G'
  | c == '9' || c == 'F' = 'F'
  | otherwise = succ c

increase :: V2 Int -> CharMatrix -> CharMatrix
increase p mat = mat Arr.// [(p, succ' (mat Arr.! p))]

step :: [V2 Int] -> CharMatrix -> (CharMatrix, Int)
step toIncrease mat =
  if null toFlash
    then (fmap f unFlashedMat, 0)
    else (+ length toFlash) <$> step newToIncrease (fmap g unFlashedMat)
  where
    unFlashedMat = foldr increase mat toIncrease
    toFlash = map fst . filter ((== 'F') . snd)
                      . Arr.assocs $ unFlashedMat
    newToIncrease = concatMap (susedi8 (Just bnds)) toFlash
    bnds = Arr.bounds mat
    f = \case
      'G' -> '0'
      c -> c
    g = \case
      'F' -> 'G'
      c -> c

solveA mat = snd $ iterate f (mat, 0) !! 100
  where
    f (mat', s) = (+s) <$> step (Arr.indices mat) mat'

solveB mat = map snd . take 10 . filter (allFlash . fst) 
             $ zip (iterate f mat) [0..]
  where
    allFlash = Arr.elems >>> all (== '0')
    f mat' = fst $ step (Arr.indices mat) mat'

main11 :: IO ()
main11 = do
  input <- parseMatrix <$> readFile "res/input11"
  print $ solveA input
  print $ solveB input
