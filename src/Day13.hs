module Day13 (main13) where

import Util
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Scanf
import qualified Data.Map as Map

data Fold = FoldX Int | FoldY Int
          deriving (Show, Eq, Ord)

type IT = (Set (V2 Int), [Fold])

parsePoints = map (splitOn "," >>> map read >>> (\(a:b:_) -> V2 a b)) >>> Set.fromList

parseFolds = mapMaybe $ \str -> do
  (c :+ (x :+ ())) <- scanf [fmt|fold along %c=%d|] str
  return $ case c of {'x' -> FoldX; 'y' -> FoldY} $ x

fold :: Fold -> Set (V2 Int) -> Set (V2 Int)
fold (FoldX x) mat = Set.union folded rest
  where
    (toFold, rest) = Set.partition (\(V2 x1 _) -> x1 > x) mat
    folded = Set.map (\(V2 x1 y1) -> V2 (x - (x1 - x)) y1) toFold
fold (FoldY y) mat = mat & (Set.map flipV2 >>> fold (FoldX y) >>> Set.map flipV2)
  where
    flipV2 (V2 x1 y1) = V2 y1 x1

toColor = \case
  1 -> PixelRGB8 0 0 0
  0 -> PixelRGB8 255 255 255

solveA :: IT -> Int
solveA (points, folds) = Set.size $ fold (head folds) points

solveB :: IT -> IO ()
solveB (points, folds) = generateBlackAndWhiteImage "res/output13.bmp" 0 mapa toColor
  where
    finalPoints = foldl (flip fold) points folds
    mapa = Map.fromList $ zip (Set.toList finalPoints) (repeat 1)

main13 :: IO ()
main13 = do
    input <- (\(a:b:_) -> (parsePoints a, parseFolds b)) . map lines . take 2 . splitOn "\n\n" <$> readFile "res/input13"
    print $ solveA input
    solveB input
