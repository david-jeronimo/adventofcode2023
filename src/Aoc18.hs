{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc18 (parseInput, solution) where

import Lib (Part(..), Text, CyclicEnum(..))
import qualified Data.Text as T
import Data.List.Extra (nub, sort, find, groupSortOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import Data.Text.Read (hexadecimal)
import Data.Maybe (isJust)
import Data.Bifunctor (first, second, bimap)
import Data.Tuple.Extra (both)

data Dir = R | D | L | U deriving (Show,Read,Eq,Ord,Bounded,Enum,CyclicEnum)
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
solution part movements = pathTiles + innerTiles
  where path = case part of
          PartOne -> foldl followPath [(0,0)] movements
          PartTwo -> foldl followPath [(0,0)] . map (parseColour . colour) $ movements
        pathTiles = sum . zipWith (\(x1,y1) (x2,y2) -> abs (x1-x2) + abs(y1-y2)) path $ tail path
        innerTiles = sum . map (areaSize mat) . S.toList $ innerPos
        mat = M.fromLists $ areasMat path
        innerPos = innerPositions  . map fst . filter (isInPath path . snd) . M.toList . M.mapPos (addRelativePos path) $ mat

followPath::Path -> Movement -> Path
followPath (pos:path) Movement{..} = (:pos:path) . move dir steps $ pos
followPath _ _                     = []

move::Dir -> Int -> Pos -> Pos
move U n = second ((+) . negate $ n)
move R n = first (+n)
move D n = second (+n)
move L n = first ((+) . negate $ n)

areaSize::Matrix Pos -> Pos -> Int
areaSize mat (x,y) = case (mat  M.! (x+1,y+1), mat M.! (x+2,y+2)) of
      ((x1,y1), (x2,y2)) -> (y2 - y1) * (x2 - x1)

areasMat::Path -> [[Pos]]
areasMat path = groupSortOn snd $ (,) <$> xp <*> yp
  where (xp, yp) = both (init . tail . nub . sort) . unzip . concatMap (\(x,y)->[(x+1,y),(x-1,y),(x,y-1),(x,y+1)]) $ path

addRelativePos::[Pos]->(Int,Int) -> (Int,Int) -> (Pos,Pos)
addRelativePos path (j,i) (x,y) | (x,j) `elem` path = ((j-1,i-1),(x,y))
                    | otherwise         = ((j-1,i-1),(x,y))

isInPath::Path -> Pos -> Bool
isInPath path (x,y) = isJust . find inRange . zip path $ last path : path
  where inRange ((x1,y1),(x2,y2)) = (x `between` (x1,x2) && y == y1) || (y `between` (y1,y2) && x == x1)
        a `between` (b,c) = (a >= b && a <= c) || (a <= b && a >= c)

innerPositions::[Pos] -> Set Pos
innerPositions path = enclosingArea path' inside outside
  where path' = S.fromList path
        inside = S.fromList ((,) <$> [minX..maxX] <*> [minY..maxY]) S.\\ path'
        outside = S.fromList [(x,y) | x <- [minX-1..maxX+1], y <- [minY-1..maxY+1], x `elem` [minX-1,maxX+1] || y `elem` [minY-1,maxY+1]]
        (maxX,maxY) = both maximum . unzip $ path
        (minX,minY) = both minimum . unzip $ path

enclosingArea::Set Pos -> Set Pos -> Set Pos -> Set Pos
enclosingArea path inside outside
     | null outside = inside'
     | otherwise    = enclosingArea path inside' outside'
  where (inside', outside') = (inside S.\\ outsideAdj, inside `S.intersection` outsideAdj)
        outsideAdj = S.fromList ([first,second] <*> [pred,succ] <*> S.toList outside) S.\\ path

-- 3274