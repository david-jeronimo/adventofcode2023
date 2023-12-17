{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc17 (parseInput, solution) where

import Lib (Part(..), Text, CyclicEnum(..))
import qualified Data.Text as T
import Data.Matrix (Matrix, nrows, ncols)
import qualified Data.Matrix as M
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first, second)
import Algorithm.Search (dijkstraAssoc)

data Dir = N | E | S | W deriving (Show,Eq,Ord,Bounded,Enum,CyclicEnum)
type Pos = (Int,Int)
data Crucible = Crucible { pos::Pos, dir::Dir, straightSteps::Int } deriving (Eq,Ord)

parseInput::Text -> Matrix Int
parseInput = M.fromLists . map (map digitToInt . T.unpack) . T.lines

solution::Part -> Matrix Int -> Maybe Int
solution part mat = fst <$> dijkstraAssoc (neighbours part mat) isSolution initState
  where isSolution Crucible{..} = case part of
          PartOne -> pos == (nrows mat, ncols mat)
          PartTwo -> pos == (nrows mat, ncols mat) && straightSteps >= 4
        initState = Crucible (1,1) E 0

neighbours::Part -> Matrix Int -> Crucible -> [(Crucible, Int)]
neighbours part mat Crucible{..} = mapMaybe neighbourCost $ case part of
    PartOne | straightSteps == 3  -> [turnLeft, turnRight]
            | otherwise           -> [turnLeft, turnRight, goStraight]
    PartTwo | straightSteps == 10 -> [turnLeft, turnRight]
            | straightSteps >= 4  -> [turnLeft, turnRight, goStraight]
            | otherwise           -> [goStraight]
  where neighbourCost c@Crucible{pos=(y,x)} = (c,) <$> M.safeGet y x mat
        turnLeft   = Crucible (move (predC dir) pos) (predC dir) 1
        turnRight  = Crucible (move (succC dir) pos) (succC dir) 1
        goStraight = Crucible (move dir pos) dir (straightSteps + 1)
        move N = first pred
        move E = second succ
        move S = first succ
        move W = second pred