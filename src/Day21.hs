module Day21 (main21) where

import Util
import Data.Maybe (mapMaybe)
import Text.Scanf
import qualified Data.Map as Map
import Data.Function.Memoize
import Control.Monad (replicateM)

parse str = do
  (d :+ x :+ ()) <- scanf [fmt|Player %d starting position: %d|] str
  return (d, x)

dieSize = 100
mapSize = 10
pointsLimit = 1000

addWithRollOver :: Int -> Int -> Int -> Int
addWithRollOver n x = pred >>> (+x) >>> (`mod` n) >>> succ

data GameState = GameState { pos :: (Int, Int),
                             points :: (Int, Int) }

step :: Int -> GameState -> GameState
step roll GameState { pos=(pos1, pos2), points=(points1, points2) } =
  GameState { pos = (pos2, newPos),
              points = (points2, points1 + newPos) }
  where
    newPos = addWithRollOver mapSize roll pos1

playerWon :: Int -> GameState -> Bool
playerWon limit = (>= limit) . snd . points

solveA :: GameState -> Int
solveA initialState = loserPoints * (3 * (length iters - 1))
  where
    dieStates = iterate (addWithRollOver dieSize 1) 1
    (_, GameState { points = (loserPoints, _) }) = last iters
    iters = iterateUntil (snd >>> snd >>> playerWon pointsLimit) go (dieStates, initialState)
    go (dieStates', gameState) = let (sum -> roll, dieStates'') = splitAt 3 dieStates'
                                  in (dieStates'', step roll gameState)

pointsLimitB = 21
dieSizeB = 3

deriveMemoizable ''GameState

solveB :: GameState -> V2 Integer
solveB = go' (1::Int)
  where
    go' = memoFix2 $ \go turn gameState
           -> sum $ do
            roll <- sum <$> replicateM 3 [1..dieSizeB]
            let gameState' = step roll gameState
            return $ if playerWon pointsLimitB gameState'
               then case turn of
                      1 -> V2 1 0
                      2 -> V2 0 1
               else go (3-turn) gameState'

main21 :: IO ()
main21 = do
    input <- Map.fromList . mapMaybe parse . lines <$> readFile "res/input21"
    let initialState = GameState { pos = (input Map.! 1, input Map.! 2),
                                   points = (0, 0) }
    print $ solveA initialState
    print $ maximum $ solveB initialState
