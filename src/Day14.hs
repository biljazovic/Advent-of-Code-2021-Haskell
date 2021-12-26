module Day14 (main14) where

import Util
import qualified Data.Map as Map
import Data.Map (Map)

parseRules :: String -> Map String Char
parseRules = lines >>> map (splitOn " -> " >>> \(a:[b]:_) -> (a, b)) >>> Map.fromList

step :: Map String Char -> Map String Integer -> Map String Integer
step mapa freqs = Map.fromListWith (+) newFreqs
  where
    f par@([c1,c2]) = case mapa Map.!? par of
              Just c -> [ [c1,c], [c,c2] ]
              Nothing -> [ par ]
    newFreqs = concatMap (\(par, s) -> map (, s) (f par)) $ Map.assocs freqs

solve :: Int -> (String, Map String Char) -> Integer
solve n (startStr, mapa) = maximum endFreqs - minimum endFreqs
  where
    startParFreqs = zipWith (\c1 c2 -> [c1,c2]) startStr (tail startStr) 
                        & freqMap
                        & Map.map toInteger
    endParFreqs = iterate (step mapa) startParFreqs !! n
    endFreqs = endParFreqs & Map.assocs
                           & map (\([_,c],s) -> (c, s)) & (++) [(head startStr, 1)]
                           & Map.fromListWith (+) & Map.assocs
                           & map snd

solveA = solve 10
solveB = solve 40

main14 :: IO ()
main14 = do
    input <- (\(a:b:_) -> (a, parseRules b)) . splitOn "\n\n" <$> readFile "res/input14"
    print $ solveA input
    print $ solveB input
