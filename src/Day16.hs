module Day16 (main16) where

import Util
import Data.Digits (unDigits, digits)
import Data.Char (digitToInt)
import Text.Megaparsec

type Parser = Parsec Void [Int]

data Packet = LiteralPacket { version :: Int, value :: Integer }
            | OperatorPacket { version :: Int, id :: Int, subPackets :: [Packet] }
            deriving (Show, Eq)

parseBit :: Parser Int
parseBit = single 1 <|> single 0

parseFixedBinary :: Int -> Parser Int
parseFixedBinary n = unDigits 2 <$> count n parseBit

parsePacket :: Parser Packet
parsePacket = do
  ver <- parseFixedBinary 3
  _id <- parseFixedBinary 3
  if _id == 4 
     then do
      val <- (++) . concat <$> many (single 1 *> count 4 parseBit)
                               <*> (single 0 *> count 4 parseBit)
      return (LiteralPacket ver (toInteger $ unDigits 2 val))
     else do
      lengthTypeId <- parseBit
      subP <- case lengthTypeId of
        0 -> do
          len <- parseFixedBinary 15
          toParse <- count len parseBit
          let Just parsed = parseMaybe (some parsePacket) toParse
          return parsed
        1 -> do
          len <- parseFixedBinary 11
          count len parsePacket
      return (OperatorPacket ver _id subP)


padTo4Multiple :: [Int] -> [Int]
padTo4Multiple xs = (replicate r 0) ++ xs
  where
    len = length xs
    r = if len `mod` 4 == 0
           then 0
           else 4 - (len `mod` 4)

solveA :: Packet -> Int
solveA (LiteralPacket ver _) = ver
solveA (OperatorPacket ver _ subP) = ver + sum (map solveA subP)

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary d = digits 2 d

solveB :: Packet -> Integer
solveB (LiteralPacket _ val) = val
solveB (OperatorPacket _ _id subP) = 
  let vals = map solveB subP
   in case _id of
        0 -> sum vals
        1 -> product vals
        2 -> minimum vals
        3 -> maximum vals
        5 -> let [a,b] = vals
              in if a > b then 1 else 0
        6 -> let [a,b] = vals
              in if a < b then 1 else 0
        7 -> let [a,b] = vals
              in if a == b then 1 else 0

main16 :: IO ()
main16 = do
    Just input <- parseMaybe (parsePacket <* many parseBit) 
                . concatMap (padTo4Multiple . toBinary . unDigits 16 . (: []). digitToInt) 
                . head . lines <$> readFile "res/input16"
    print $ solveA input
    print $ solveB input
