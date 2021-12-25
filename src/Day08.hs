module Day08 (main08) where

import Util
import Data.Char (ord)
import Data.Maybe (mapMaybe)
import Data.Digits (unDigits)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

parseLine :: String -> ([String], [String])
parseLine str = (s1, s2)
  where
    [s1, s2] = map (filter (not . emptyLine) . splitOn " ") $ filter (not . emptyLine) $ splitOn "|" str

solveA = listCount ((`elem` [2,4,3,7]) . length) . concatMap snd

digits = Map.fromList $ zip [ "abcefg", "cf", "acdeg", "acdfg", "bcdf",
                              "abdfg", "abdefg", "acf", "abcdefg", "abcdfg" ] 
                            [0..]

getDigit :: String -> Maybe Int
getDigit (sort -> str) = digits Map.!? str

charToInt :: Char -> Int
charToInt c = ord c - ord 'a'

convertWithPerm :: String -> String -> String
convertWithPerm perm = map ((perm !!) . charToInt)

convertsWithPerm :: String -> [String] -> Maybe [Int]
convertsWithPerm perm strs = if length ds == length strs
                                then Just ds
                                else Nothing
  where
    ds = mapMaybe (getDigit . convertWithPerm perm) strs

solveB (map (uncurry (++)) -> it) = sum $ map calc it
  where
    perms = permutations ['a'..'g']
    calc strs = let ds = head $ mapMaybe (`convertsWithPerm` strs) perms
                 in unDigits 10 (drop 10 ds)


main08 :: IO ()
main08 = do
    input <- map parseLine . lines <$> readFile "res/input08"
    print $ solveA input
    print $ solveB input
