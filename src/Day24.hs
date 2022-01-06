{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day24 (main24) where

import Util
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toUpper)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (forM_)

type Parser = Parsec Void String

type Var = Char
data Op = Add | Div | Eql | Mul | Mod
  deriving (Show, Eq, Read)
data Instr = Input Var | Operation Op Var (Either Int Var)
  deriving (Show, Eq)

lexeme :: Parser a -> Parser a
lexeme p = p <* space 

parseInstr :: Parser Instr
parseInstr = (try parseInput <|> parseOperation) <* space
  where
    parseInput = do
      lexeme (string "inp")
      Input <$> anySingle
    parseOperation = do
      opStr <- someTill anySingle space1
      let opStr' = toUpper (head opStr) : tail opStr
      c <- lexeme anySingle
      s <- eitherP signedInteger anySingle
      return $ Operation (read opStr') c s
    signedInteger = L.signed space L.decimal

eval a b = \case
  Mul -> a * b
  Add -> a + b
  Div -> a `div` b
  Mod -> a `mod` b
  Eql -> fromEnum $ a==b

type ProgramState = (Map Char Int, [Int])

runInstr :: Instr -> ProgramState -> ProgramState
runInstr (Input v) (vars, x:xs) = (Map.insert v x vars, xs)
runInstr (Operation op v1 vx2) (vars, xs) = (Map.insert v1 (eval (getVar v1) (toValue vx2) op) vars, xs)
  where
    toValue (Left x) = x
    toValue (Right v) = getVar v
    getVar v = Map.findWithDefault 0 v vars

runInstrs :: [Instr] -> ProgramState -> ProgramState
runInstrs instrs s1 = foldl (flip runInstr) s1 instrs

getInstrBatch :: [Instr] -> [Instr]
getInstrBatch (i : is) = i : takeWhile isOperation is
  where
    isOperation (Input _) = False
    isOperation _ = True

getBatches :: [Instr] -> [[Instr]]
getBatches [] = []
getBatches is = let iis = getInstrBatch is
                 in iis : getBatches (drop (length iis) is)

runBatches :: [[Instr]] -> [Int] -> [Int]
runBatches iis xs = go iis (Map.empty, xs)
  where
    go [] _  = []
    go (is:iis') st = let st' = runInstrs is st
                       in (fst st' Map.! 'z') : go iis' st'

main24 :: IO ()
main24 = do
    input <- mapMaybe (parseMaybe parseInstr) . lines <$> readFile "res/input24"
    let batches = getBatches input
        zs = runBatches batches [6,1,1,9,1,5,1,6,1,1,1,3,2,1]
    forM_ (zip3 [1..] zs (map (`mod` 26) zs)) print















