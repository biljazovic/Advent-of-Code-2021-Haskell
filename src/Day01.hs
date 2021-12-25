module Day01 (main01) where

import Util (emptyLine)

type IT = [Int]

parse :: String -> IT
parse = map read . filter (not . emptyLine) .lines

solveA :: IT -> Int
solveA arr = sum . map (\(x, y) -> if y > x then 1 else 0) $ zip arr (tail arr)

solveB :: IT -> Int
solveB arr = solveA arr2 
  where
    arr2 = map (\(x, y, z) -> x + y + z) $ zip3 arr (tail arr) (tail (tail arr))

main01 :: IO ()
main01 = do
  input <- parse <$> readFile "res/input01"
  print $ solveA input
  print $ solveB input
