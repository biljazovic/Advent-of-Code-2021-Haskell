module Day03 (main03) where

import Util (listCount, emptyLine)
import Data.Digits (unDigits)

parse :: String -> [[Int]]
parse = map (map (\c -> read [c])) . filter (not . emptyLine) . lines

solveA :: [[Int]] -> Integer
solveA it = product $ map (unDigits 2) [gammaDigits, epsDigits]
  where
    oneCounts = map (\i -> listCount (== 1) (map (!! i) it)) [0..n-1]
    gammaDigits = map (\i -> if i >= m then 1 else 0) oneCounts
    epsDigits = map (1 -) gammaDigits
    m = length it `div` 2
    n = length $ head it

solveB :: [[Int]] -> Int
solveB it = oxygenValue * co2Value
  where
    oneCounts i = listCount (== 1) . map (!! i)
    mostCommonDigit i xs = if oneCounts i xs >= (length xs + 1) `div` 2 
                               then 1 else 0
    leastCommonDigit = ((1 -) . ) . mostCommonDigit
    go condition i xs = if length xs == 1
                 then head xs
                 else let k = condition i xs
                          xs' = filter ((== k) . (!! i)) xs
                       in go condition (i+1) xs'
    oxygenValue = unDigits 2 (go mostCommonDigit 0 it)
    co2Value = unDigits 2 (go leastCommonDigit 0 it)

main03 :: IO ()
main03 = do
    input <- parse <$> readFile "res/input03"
    print $ solveA input
    print $ solveB input
