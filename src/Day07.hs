module Day07 (main07) where

import Util ( sort, splitOn )
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

solveA it = let sit = sort it
                cost s = sum $ map (abs . (s-)) it
             in cost (sit !! (length sit `div` 2))

solveB it = minimum $ map calc [min_x .. max_x]
  where
    calc s = sum $ map (\x -> let a = abs (x-s) in a*(a+1) `div` 2) it
    min_x = minimum it
    max_x = maximum it


main07 :: IO ()
main07 = do
    input <- map read . splitOn "," . head . lines <$> readFile "res/input07" :: IO [Int]
    print $ solveA input
    print $ solveB input
