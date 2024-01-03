{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Aoc18 (parseInput, solution) where

import Lib (Part(..), Text, innerPositions)
import qualified Data.Text as T
import Data.List.Extra (nub, sort, groupSortOn)
import qualified Data.Set as S
import qualified Data.Matrix as M
import Data.Text.Read (hexadecimal)
import Data.Bifunctor (first, second, bimap)
import Data.Tuple.Extra (both)

data Dir = R | D | L | U deriving (Show,Read,Enum)
data Movement = Movement { dir::Dir, steps::Int, colour::Text} deriving Show
type Pos = (Int,Int)
type Path = [Pos]

parseInput::Text -> [Movement]
parseInput txt = [Movement (read . T.unpack $ d) (read . T.unpack $ s) c | [d,s,c] <- T.words <$> T.lines txt]

parseColour::Text -> Movement
parseColour = toMovement . bimap numSteps parseDir . T.splitAt 5 . T.init . T.drop 2
  where numSteps = either (const 0) fst . hexadecimal
        parseDir = toEnum . read . T.unpack
        toMovement (s,d) = Movement d s T.empty

solution::Part -> [Movement] -> Int
solution part movements = pathTiles + innerTilesSize path
  where path = scanl followPath (0,0) $ case part of
          PartOne -> movements
          PartTwo -> map (parseColour . colour) movements
        followPath pos Movement{..} = move dir steps pos
        pathTiles = sum . zipWith dist path $ tail path
        dist (x1,y1) (x2,y2) = abs (x1-x2) + abs(y1-y2)

move::Dir -> Int -> Pos -> Pos
move U n = second (subtract n)
move R n = first (+n)
move D n = second (+n)
move L n = first (subtract n)

innerTilesSize::Path -> Int
innerTilesSize path = sum . map areaSize $ innerTiles
  where mat = M.fromLists $ areasMat path
        innerTiles = S.toList . innerPositions . translateTiles $ mat
        translateTiles = map fst . filter (isInPath path . snd) . M.toList . M.mapPos addRelativePos
        addRelativePos (j,i) = ((j-1,i-1), )
        areaSize (x,y) = case (mat M.! (x+1,y+1), mat M.! (x+2,y+2)) of
              ((x1,y1), (x2,y2)) -> (y2 - y1) * (x2 - x1)
  
areasMat::Path -> [[Pos]]
areasMat path = groupSortOn snd $ (,) <$> xp <*> yp
  where (xp, yp) = both (init . tail . nub . sort) . unzip . concatMap adj $ path
        adj pos = [first,second] <*> [pred,succ] <*> pure pos
  
isInPath::Path -> Pos -> Bool
isInPath path (x,y) = any inRange . zip path $ last path : path
  where inRange ((x1,y1),(x2,y2)) = (x `between` (x1,x2) && y == y1) || (y `between` (y1,y2) && x == x1)
        a `between` (b,c) = (a >= b && a <= c) || (a <= b && a >= c)