module Day18 (main18) where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Maybe (mapMaybe)

type Parser = Parsec Void String

data Pair = Regular { value :: Int } | Pair { left :: Pair, right :: Pair }
  deriving (Show, Eq)

parser :: Parser Pair
parser = (Regular <$> decimal)
  <|> (\(x:y:_) -> Pair x y) <$> between (char '[') (char ']') (sepBy1_ parser (char ','))

addLeftest :: Int -> Pair -> Pair
addLeftest a (Regular x) = Regular (x+a)
addLeftest a (Pair l r) = Pair (addLeftest a l) r

addRightest :: Int -> Pair -> Pair
addRightest a (Regular x) = Regular (x+a)
addRightest a (Pair l r) = Pair l (addRightest a r)

explodePair :: Pair -> Maybe Pair
explodePair pair = fmap (\(p, _, _) -> p) (go 0 pair)
  where
    go :: Int -> Pair -> Maybe (Pair, Int, Int)
    go depth pair' = 
      case pair' of
        Regular _ -> Nothing
        Pair l r -> if depth == 4
         then Just (Regular 0, value l, value r)
         else case go (depth+1) l of
           Just (newL, toAddLeft, toAddRight) -> Just (Pair newL (addLeftest toAddRight r), toAddLeft, 0)
           Nothing -> case go (depth+1) r of
             Just (newR, toAddLeft, toAddRight) -> Just (Pair (addRightest toAddLeft l) newR, 0, toAddRight)
             Nothing -> Nothing

splitPair :: Pair -> Maybe Pair
splitPair (Regular x) = if x >= 10
                           then Just (Pair (Regular (x `div` 2)) (Regular ((x+1) `div` 2)))
                           else Nothing
splitPair (Pair l r) = case splitPair l of
                         Just newL -> Just (Pair newL r)
                         Nothing -> case splitPair r of
                                      Just newR -> Just (Pair l newR)
                                      Nothing -> Nothing

reducePair :: Pair -> Pair
reducePair pair = maybe (maybe pair reducePair (splitPair pair))
                        reducePair (explodePair pair) 

magnitude :: Pair -> Integer
magnitude (Regular x) = toInteger x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

solveA :: [Pair] -> Integer
solveA = magnitude . foldl1 f
  where
    f p1 p2 = reducePair (Pair p1 p2)

solveB :: [Pair] -> Integer
solveB pairs = maximum $ [magnitude (reducePair $ Pair p1 p2) | p1 <- pairs, p2 <- pairs, p1 /= p2]

main18 :: IO ()
main18 = do
    input <- mapMaybe (parseMaybe parser) . lines <$> readFile "res/input18"
    print $ solveA input
    print $ solveB input

