module Day20 (main20) where

import Util
import Data.Digits (unDigits)
import qualified Data.Array as Arr
import Data.Ix (inRange)

type Image = (CharMatrix, Char)

type IT = (String, CharMatrix)

allBetween :: (V2 Int, V2 Int) -> [V2 Int]
allBetween (V2 x1 y1, V2 x2 y2) = [ V2 x y | x <- [x1..x2], y <- [y1..y2] ]

arrLookup :: Arr.Ix i => e -> Arr.Array i e -> i -> e
arrLookup d arr p = if inRange (Arr.bounds arr) p
                     then arr Arr.! p
                     else d

expandEmpty :: Image -> CharMatrix
expandEmpty (img, c) = img'
  where
    (ul, dr) = Arr.bounds img
    bounds' = (ul - V2 1 1, dr + V2 1 1)
    img' = Arr.array bounds' $ map (\p -> (p, toChar p)) (allBetween bounds')
    toChar p = arrLookup c img p

mySusedi :: V2 Int -> [V2 Int]
mySusedi (V2 x y) = concat [ [ V2 x' y' | y' <- [y-1..y+1] ] | x' <- [x-1..x+1] ]

code :: Image -> V2 Int -> Int
code (img, c) p = mySusedi p 
                  & map (arrLookup c img) 
                  & map toBit
                  & unDigits 2

toBit :: Char -> Int
toBit = \case
  '#' -> 1
  '.' -> 0

convertEmpty :: String -> Char -> Char
convertEmpty codeBook c = codeBook !! unDigits 2 (replicate 9 (toBit c))

step :: String -> Image -> Image
step codeBook (img, c) = (img', c') 
  where
    c' = convertEmpty codeBook c
    emptyImg' = expandEmpty (img, c)
    convert (i, _) = (i, codeBook !! code (img, c) i)
    img' = Arr.assocs emptyImg'
             & map convert 
             & Arr.array (Arr.bounds emptyImg')

solve :: Int -> IT -> Int
solve n (codeBook, startMat) = Arr.assocs finalMat 
                               & map snd 
                               & listCount (=='#') 
  where
    finalMat = fst $ iterate go startImg !! n
    go = step codeBook
    startImg = (startMat, '.')

solveA = solve 2
solveB = solve 50

main20 :: IO ()
main20 = do
    input <- (\(a:b:_) -> (concat (lines a), parseMatrix b)) . splitOn "\n\n"
            <$> readFile "res/input20"
    print $ solveA input
    print $ solveB input
