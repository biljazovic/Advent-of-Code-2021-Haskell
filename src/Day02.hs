module Day02 (main02) where

import Text.ParserCombinators.ReadP
import Util (parseInt, newline)
import Control.Applicative hiding (many)

data Command = Forward Int | Down Int | Up Int
            deriving (Show, Eq)

parser :: ReadP [Command]
parser = many $ (parseForward <|> parseDown <|> parseUp) <* many newline
  where
    parseForward = Forward <$> (string "forward " *> parseInt)
    parseDown = Down <$> (string "down " *> parseInt)
    parseUp = Up <$> (string "up " *> parseInt)

solveA :: [Command] -> Int
solveA = (uncurry (*)) . foldl f (0, 0)
  where
    f (x, y) (Forward z) = (x + z, y)
    f (x, y) (Down z) = (x, y + z)
    f (x, y) (Up z) = (x, y - z)

solveB :: [Command] -> Integer
solveB = (\(x, y, _) -> x * y) . foldl f (0, 0, 0)
  where
    f (x, y, a) (Forward (toInteger -> z)) = (x + z, y + a*z, a)
    f (x, y, a) (Down (toInteger -> z)) = (x, y, a + z)
    f (x, y, a) (Up (toInteger -> z)) = (x, y, a - z)

main02 :: IO ()
main02 = do
  [(input, _)] <- readP_to_S (parser <* eof) <$> readFile "res/input02"
  print $ solveA input
  print $ solveB input
    

