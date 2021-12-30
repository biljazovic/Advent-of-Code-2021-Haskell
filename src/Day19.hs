module Day19 (main19) where

import Util
import Linear.V3
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Strict

type Scanner = [V3 Int]

parseV3 :: String -> V3 Int
parseV3 str = let [a,b,c] = map read $ splitOn "," str
               in V3 a b c

fingerPrint :: Scanner -> MultiSet (V3 Int)
fingerPrint points = MultiSet.fromList
  $ filter (> V3 0 0 0)
  $ [ p1 - p2 | p1 <- points, p2 <- points, p1 /= p2 ]

rotations :: V3 Int -> [V3 Int]
rotations v = concat [ allTurns v' | v' <- take 4 (iterate roll v) ]
              ++ allTurns (roll . turn $ v)
              ++ allTurns (roll . turn . turn . turn $ v)
  where
    allTurns = take 4 . iterate turn
    turn (V3 x y z) = V3 (-y) x z
    roll (V3 x y z) = V3 x z (-y)

scannerRotations :: Scanner -> [Scanner]
scannerRotations = transpose . map rotations

matchPair :: Scanner -> Scanner -> Maybe (Scanner, V3 Int)
matchPair s0 s1 = if pairMatch s0 s1 
                     then Just $ head $ do
                        s1' <- scannerRotations s1
                        p' <- s1'
                        p <- s0
                        let s1'' = map (+ (p - p')) s1'
                        guard (Set.size (Set.intersection (Set.fromList s1'') (Set.fromList s0)) >= numPointsToMatch)
                        return (s1'', p - p')
                     else Nothing

numPointsToMatch = 12
numPairsToMatch = numPointsToMatch*(numPointsToMatch-1) `div` 2

pairMatch :: Scanner -> Scanner -> Bool
pairMatch s1 s2 = any (f s1) (scannerRotations s2)
  where
    f s1' s2' = MultiSet.size (MultiSet.intersection (fingerPrint s1') (fingerPrint s2')) >= numPairsToMatch

data MyState = MyState { _scanners :: Map Int Scanner, 
                         _scannerPos :: Map Int (V3 Int),
                         _beacons :: Set (V3 Int) }

solve :: [Scanner] -> MyState
solve scanners = execState comp initState
  where
    indices = [0..length scanners - 1]
    initState = MyState (Map.fromList (zip [0..] scanners)) 
                 (Map.singleton 0 (V3 0 0 0)) 
                 (Set.fromList (head scanners))
    comp = go 0
    go :: Int -> State MyState ()
    go ind = do
      forM_ indices $ \ind' -> do
        MyState newScanners scannerPos beacons <- get
        if ind' `Map.member` scannerPos
           then return ()
           else case matchPair (newScanners Map.! ind) (newScanners Map.! ind') of
             Nothing -> return ()
             Just (s', pos') -> do
               put $ MyState (Map.insert ind' s' newScanners)
                    (Map.insert ind' pos' scannerPos)
                    (Set.union beacons (Set.fromList s'))
               go ind'

solveA = Set.size . _beacons

solveB = maximum . map (uncurry manhattan) . unorderedPairs . Map.elems . _scannerPos
  where
    manhattan (V3 x1 y1 z1) (V3 x2 y2 z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

main19 :: IO ()
main19 = do
    input <- map (map parseV3 . drop 1 . lines) . splitOn "\n\n" <$> readFile "res/input19" :: IO [Scanner]
    let finalState = solve input
    print $ solveA finalState
    print $ solveB finalState
