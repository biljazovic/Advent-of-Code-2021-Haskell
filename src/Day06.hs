module Day06 (main06) where

import Util
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

solve :: Int -> [Int] -> Integer
solve n input = sum $ IntMap.elems end
  where
    end = iterate go start !! n
    go freqs = let f i
                    | i == 0 = [ 6, 8 ]
                    | otherwise = [ i-1 ]
                in IntMap.fromListWith (+) $ concatMap (\(i, x) -> map (, x) (f i))
                                           $ IntMap.assocs freqs
    start = IntMap.fromListWith (+) (zip input (repeat 1)) :: IntMap Integer

solveA :: [Int] -> Integer
solveA = solve 80

solveB :: [Int] -> Integer
solveB = solve 256

main06 :: IO ()
main06 = do
    input <- map read . splitOn "," . head . lines <$> readFile "res/input06"
    print $ solveA input
    print $ solveB input
