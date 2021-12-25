module Day10 (main10) where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Foldable (toList)
import Data.Digits (unDigits)

type Parser = Parsec Void String

parser :: Parser ()
parser = many (do
  let f c1 c2 = between (char c1) (char c2) parser
  f '(' ')' <|> f '[' ']' <|> f '{' '}' <|> f '<' '>')
   $> ()

data Verdict = Legal | Corrupted Char | Incomplete Char

verdict :: String -> Verdict
verdict str = case parse parser "" str of
  (Left (NE.head . bundleErrors -> (TrivialError _ (Just errorItem) errorItems))) -> case errorItem of
    Tokens (c :| _) -> Corrupted c
    EndOfInput -> errorItems & toList & map (\(Tokens (c :| _)) -> c) & filter (`elem` ")]}>") & head & Incomplete
  _ -> Legal

solveA it = sum $ [ score c | Corrupted c <- map verdict it ]
  where
    score = \case
      ')' -> 3
      ']' -> 57
      '}' -> 1197
      '>' -> 25137

solveB :: [String] -> Integer
solveB it = sols !! (length sols `div` 2)
  where
    sols = sort $ mapMaybe g it
    g str = let dgs = f str
             in if null dgs
                   then Nothing
                   else Just $ unDigits 5 dgs
    f str = case verdict str of
                 Incomplete c -> score c : f (str ++ [c])
                 _ -> []
    score = \case
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4

main10 :: IO ()
main10 = do
    input <- lines <$> readFile "res/input10"
    print $ solveA input
    print $ solveB input
