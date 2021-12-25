module Day09 (main09) where

import Util
import Data.Char (digitToInt)
import qualified Data.Array as Arr
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

solveA :: CharMatrix -> Int
solveA mat = lowPoints >>> map ((+1) . digitToInt . (mat Arr.!))
                       >>> sum $ mat

lowPoints :: CharMatrix -> [V2 Int]
lowPoints mat = filter lowPoint $ Arr.indices mat
  where
    lowPoint ind = all ((mat Arr.!) >>> ((mat Arr.! ind) <))
                      $ susedi4 (Just $ Arr.bounds mat) ind

solveB :: CharMatrix -> Int
solveB mat = product $ take 3 $ reverse $ sort $ map (Set.size . fst) $ Map.elems endMapa
  where
    startMapa = Map.fromList $ [ (p, (Set.singleton p, Set.singleton p)) | p <- lowPoints mat ]
    endMapa = go startMapa
    go mapa = if all (Set.null . snd) (Map.elems mapa)
                 then mapa
                 else go $ Map.map f mapa
    f (visited, toSearch) = let newToSearch = Set.filter good
                                   $ Set.fromList
                                   $ concatMap (susedi4 (Just (Arr.bounds mat)))
                                   $ toSearch
                                good p = (mat Arr.! p /= '9') && (p `Set.notMember` visited)
                             in (visited `Set.union` newToSearch, newToSearch)

main09 :: IO ()
main09 = do
    input <- parseMatrix <$> readFile "res/input09"
    print $ solveA input
    print $ solveB input
