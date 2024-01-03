module Lib
    ( Part(..), Text, CyclicEnum(..), innerPositions) where

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Extra (both, first, second, (&&&))

data Part = PartOne | PartTwo deriving Show

type Pos = (Int,Int)

class (Eq a, Bounded a, Enum a) => CyclicEnum a where
  predC::a -> a
  predC a | a == minBound = maxBound
          | otherwise     = pred a
  succC::a -> a
  succC a | a == maxBound = minBound
          | otherwise     = succ a

innerPositions::[Pos] -> Set Pos
innerPositions loopPath = enclosingArea loopPath' inside outside
  where loopPath' = S.fromList loopPath
        inside = S.fromList ((,) <$> [minX..maxX] <*> [minY..maxY]) S.\\ loopPath'
        outside = S.fromList [(x,y) | x <- [minX-1..maxX+1], y <- [minY-1..maxY+1], x `elem` [minX-1,maxX+1] || y `elem` [minY-1,maxY+1]]
        ((minX,minY),(maxX,maxY)) = (both minimum &&& both maximum) . unzip $ loopPath
        
enclosingArea::Set Pos -> Set Pos -> Set Pos -> Set Pos
enclosingArea path inside outside
     | null outside = inside'
     | otherwise    = enclosingArea path inside' outside'
  where (inside', outside') = (inside S.\\ outsideAdj, inside `S.intersection` outsideAdj)
        outsideAdj = S.fromList ([first,second] <*> [pred,succ] <*> S.toList outside) S.\\ path