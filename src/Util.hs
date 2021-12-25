module Util
  ( module Linear.V2,
    module Data.Void,
    module Data.List,
    module Data.List.Split,
    module Control.Arrow,
    module Data.Function,
    module Data.Functor,
    module Codec.Picture,
    parseMatrix,
    CharMatrix,
    sepBy1_,
    emptyLine,
    generateBlackAndWhiteImage,
    generateGraph,
    susedi4,
    susedi8,
    genericBfs,
    listToArray,
    listCount,
    inBounds,
    scale,
    argmin,
    argmax,
    parseInt,
    newline,
    freqIntMap
  )
where

import Codec.Picture
    ( saveBmpImage, generateImage, DynamicImage(ImageRGB8), PixelRGB8(..) )
import Control.Lens ( (^.) )
import Control.Monad (guard)
import Data.Array ( Ix(inRange), Array, array, listArray )
import Data.Char (isSpace)
import Data.Graph.Inductive.Graph ( Graph(mkGraph) )
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Void (Void)
import Linear.V2
import Text.Megaparsec (MonadParsec, try, many)
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP
import Data.Functor (($>))
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Control.Arrow ((>>>))
import Data.Function ((&))

newline :: ReadP ()
newline = ReadP.char '\n' $> ()

parseInt :: ReadP Int
parseInt = read <$> ReadP.munch1 (`elem` ['0'..'9'])

type CharMatrix = Array (V2 Int) Char

parseMatrix :: String -> CharMatrix
parseMatrix strToParse =
  let strs = filter (not . emptyLine) $ lines strToParse
      n = length strs
      m = length $ head strs
      lst = concat $ [[(V2 i j, ch) | (ch, j) <- zip str [0 ..]] | (str, i) <- zip strs [0 ..]]
   in array (V2 0 0, V2 (n -1) (m -1)) lst

sepBy1_ :: MonadParsec e s m => m a -> m sep -> m [a]
sepBy1_ p sep = (:) <$> try p <*> many (try (sep *> p))

genericBfs ::
  Ord a =>
  (a -> Bool) -> -- goal
  (a -> [a]) -> -- neighbors
  a -> -- start
  Maybe Int -- distance from start to goal
genericBfs goal neighs start = bfs (Set.singleton start) (Seq.singleton (start, 0))
  where
    bfs seen queue = case queue of
      (Seq.viewl -> (cur, dist) Seq.:< rest) ->
        if goal cur
          then Just dist
          else
            let (newSeen, newQueue) = foldr g (seen, rest) (neighs cur)
                g neigh (seen', queue') =
                  if Set.member neigh seen'
                    then (seen', queue')
                    else (Set.insert neigh seen', queue' Seq.|> (neigh, dist + 1))
             in bfs newSeen newQueue
      _ -> Nothing

generateBlackAndWhiteImage :: String -> a -> Map (V2 Int) a -> (a -> PixelRGB8) -> IO ()
generateBlackAndWhiteImage filename defaultValue mapa toColor = saveBmpImage filename image
  where
    image = ImageRGB8 $ generateImage f (maxX - minX + 1) (maxY - minY + 1)
    f x y = toColor $ Map.findWithDefault defaultValue (V2 (x + minX) (y + minY)) mapa
    coords = Map.keysSet mapa
    coordsx = Set.map (^. _x) coords
    coordsy = Set.map (^. _y) coords
    [minX, maxX] = map ($ coordsx) [Set.findMin, Set.findMax]
    [minY, maxY] = map ($ coordsy) [Set.findMin, Set.findMax]

susedi :: [V2 Int] -> Maybe (V2 Int, V2 Int) -> V2 Int -> [V2 Int]
susedi dirs limits s = filter f $ map (+ s) dirs 
  where
    f x = case limits of
      Nothing -> True
      Just bnds -> inBounds bnds x

susedi4 :: Maybe (V2 Int, V2 Int) -> V2 Int -> [V2 Int]
susedi4 = susedi [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]

susedi8 :: Maybe (V2 Int, V2 Int) -> V2 Int -> [V2 Int]
susedi8 = susedi [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1), V2 1 1, V2 (-1) 1, V2 1 (-1), V2 (-1) (-1)]

inBounds :: (V2 Int, V2 Int) -> V2 Int -> Bool
inBounds (V2 i0 j0, V2 i1 j1) (V2 i j) = (i0, i1) `inRange` i && (j0, j1) `inRange` j

generateGraph :: Map (V2 Int) Int -> (Gr Int Int, Map (V2 Int) Int)
generateGraph mapa = (mkGraph nodes edges, indexMap)
  where
    coords = Set.toList $ Map.keysSet mapa
    indexMap = Map.fromList $ zip coords [1 ..]
    nodes = zipWith (\coord index -> (index, mapa Map.! coord)) coords [1 ..]
    edges = concatMap f coords
    f coord = mapMaybe g (susedi4 Nothing coord)
      where
        g sused = do
          tip <- Map.lookup sused mapa
          guard (tip > 0)
          return (indexMap Map.! coord, indexMap Map.! sused, 1)

emptyLine :: String -> Bool
emptyLine = all isSpace

listToArray :: [e] -> Array Int e
listToArray lst = listArray (0, length lst - 1) lst

listCount :: (a -> Bool) -> [a] -> Int
listCount f = length . filter f

scale :: (Integral a, Integral b) => a -> V2 b -> V2 b
scale s (V2 x y) = V2 (fromIntegral s * x) (fromIntegral s * y)

argmin :: Ord a => [a] -> Int
argmin xs = fromJust $ elemIndex (minimum xs) xs

argmax :: Ord a => [a] -> Int
argmax xs = fromJust $ elemIndex (maximum xs) xs

freqIntMap :: [Int] -> IntMap Int
freqIntMap = IntMap.fromListWith (+) . flip zip (repeat 1)
