module Day04 (main04) where

import Util (Void, sepBy1_, find, transpose)
import Control.Applicative ()
import Text.Megaparsec ( parseMaybe, count, many, some, Parsec )
import Text.Megaparsec.Char ( char, hspace, newline )
import Text.Megaparsec.Char.Lexer ( decimal )
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

type Parser = Parsec Void String

type Matrix = [[Int]]
type IT = ([Int], [Matrix])

parser :: Parser IT
parser = do
  nums <- sepBy1_ decimal (char ',')
  many newline
  let mat :: Parser Matrix
      mat = sepBy1_ (some (hspace *> decimal) <* hspace) newline
  mats <- sepBy1_ mat (count 2 newline)
  many newline
  return (nums, mats)

solveA :: IT -> Int
solveA (nums, mats) = go nums IntSet.empty
  where
    go (x:xs) vs = let vs' = IntSet.insert x vs
                       won = winningBoard vs' mats
                    in case won of
                            Just w -> calcResult vs' x w
                            Nothing -> go xs vs'

solveB :: IT -> Int
solveB (nums, mats) = go nums IntSet.empty mats
  where
    go (x:xs) vs mats' = let vs' = IntSet.insert x vs
                             mats'' = filter (not . isWinning vs') mats'
                          in if null mats''
                                then calcResult vs' x (head mats')
                                else go xs vs' mats''

calcResult :: IntSet -> Int -> Matrix -> Int
calcResult vs lastDrawn mat = let suma = sum $ filter (`IntSet.notMember` vs) (concat mat)
                               in suma * lastDrawn

winningBoard :: IntSet -> [Matrix] -> Maybe Matrix
winningBoard vs = find (isWinning vs)

isWinning :: IntSet -> Matrix -> Bool
isWinning vs mat = let winningVec = all (`IntSet.member` vs)
                       rows = mat
                       cols = transpose mat
                    in any winningVec (rows ++ cols)

main04 :: IO ()
main04 = do
    Just input <- parseMaybe parser <$> readFile "res/input04"
    print $ solveA input
    print $ solveB input

