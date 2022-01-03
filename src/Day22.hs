{-# LANGUAGE DataKinds #-}
module Day22 (main22) where

import Util
import Data.Maybe (mapMaybe)
import Text.Scanf
import qualified Data.Vector.Fixed as FV
import Data.Vector.Fixed.Boxed (Vec3)
import qualified Data.Bimap as Bimap
import Data.Bimap (Bimap, (!>))
import qualified Data.Massiv.Array.Mutable as MM
import Control.Monad.ST
import Data.Massiv.Array (Sz (..), U, Ix3T)
import qualified Data.Massiv.Array as Massiv
import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.Vector.Fixed ((!))

type Point = Vec3 Int
data Instr = Instr { onoff :: Bool,
                     startPoint :: Point,
                     endPoint :: Point }
  deriving (Show, Eq)
type IT = ([Instr], Vec3 (Bimap Int Int))

(<!) = (Bimap.!)

parse :: String -> Maybe Instr
parse str = do
  (s :+ x1 :+ x2 :+ y1 :+ y2 :+ z1 :+ z2 :+ ()) <- scanf [fmt|%s x=%d..%d,y=%d..%d,z=%d..%d|] str
  let onoff = case s of
            "on" -> True
            _ -> False
  return (Instr onoff (FV.fromList [x1,y1,z1]) (FV.fromList [x2,y2,z2]))

compress :: [Instr] -> ([Instr], Vec3 (Bimap Int Int))
compress instrs = (map f instrs, bimaps)
  where
    bimaps = FV.generate $ \ind ->
      let sorted = instrs & concatMap (\x -> [ startPoint x, endPoint x ])
                          & map (! ind)
                          & (++) [-50, 50]
                          & concatMap (\x -> [x, x+1])
                          & mkUniq
       in Bimap.fromList $ zip sorted [0..]
    f Instr { onoff, startPoint, endPoint } = Instr { onoff,
                                                      startPoint = g startPoint,
                                                      endPoint = g endPoint }
    g = FV.imap (\ind x -> (bimaps ! ind) <! x)

computeArr :: IT -> Massiv.Array U Ix3T Bool
computeArr (instrs, bimaps) = Massiv.setComp Massiv.Par arr
  where
    [s1,s2,s3] = FV.toList bimaps & map Bimap.size
    arr = runST $ do
      marr <- MM.newMArray (Sz (s1, s2, s3)) False
      forM_ instrs $ \Instr { onoff, startPoint, endPoint } -> do
        let f ind = [ startPoint ! ind .. endPoint ! ind ]
            indices = [ (x, y, z) | x <- f 0, y <- f 1, z <- f 2 ]
        forM_ indices $ \t -> MM.writeM marr t onoff 
      MM.freezeS marr

solveA arr (_, bimaps) = Massiv.sum $ Massiv.imap (\t b -> if b then cubeSize t else 0) extractedArr
  where
    cubeSize (x, y, z) = let f bimap xyz = bimap !> (xyz+1) - bimap !> xyz
                          in product $ zipWith f (toList bimaps) [x+x1,y+y1,z+z1]
    extractedArr = Massiv.extractFromTo' (x1,y1,z1) (x2,y2,z2) arr
    [x1,y1,z1] = map (<! (-50)) $ FV.toList bimaps
    [x2,y2,z2] = map (<! 50) $ FV.toList bimaps

solveB :: Massiv.Array U Ix3T Bool -> IT -> Integer
solveB arr (_, bimaps) = Massiv.sum $ Massiv.imap (\t b -> if b then cubeSize t else 0) arr
  where
    cubeSize (x, y, z) = let f bimap xyz = toInteger $ bimap !> (xyz+1) - bimap !> xyz
                          in product $ zipWith f (toList bimaps) [x,y,z]

-- $> main22
main22 :: IO ()
main22 = do
    input <- compress . mapMaybe parse . lines <$> readFile "res/input22"
    let arr = computeArr input
    print $ solveA arr input
    print $ solveB arr input

