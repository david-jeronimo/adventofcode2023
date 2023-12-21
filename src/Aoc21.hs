module Aoc21 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Split (divvy)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bifunctor (first,second)
import Data.Tuple.Extra (both)
type Pos = (Int,Int)

parseInput::Text -> (Set Pos, Pos, Int)
parseInput txt = (rocks, startPosition, length $ T.lines txt)
  where rocks = S.fromList [ (i,j) | (j,line) <- zip [0..] $ T.lines txt, (i,'#') <- zip [0..] $ T.unpack line ]
        startPosition = head [ (i,j) | (j,line) <- zip [0..] $ T.lines txt, (i,'S') <- zip [0..] $ T.unpack line ]

solution::Part -> (Set Pos, Pos,Int) -> Int
solution part (rocks, start, width) = case part of
    PartOne -> S.size . (!!64) . iterate (step rocks) . S.singleton $ start
    PartTwo -> fst . (!!numIterations) . iterate (findNext increaseCycle) $ (head firstLens,firstLens!!1)
  where lens = map S.size . iterate (step2 width rocks) . S.singleton $ start
        (firstLens, increaseCycle) = findIncreaseCycle totalSteps width lens
        numIterations = (totalSteps `div` width) - length firstLens + 1
        findNext ic (prev1,prev2) = (2 * prev1 - prev2 + ic,prev1)
        totalSteps = 26501365

findIncreaseCycle::Int -> Int -> [Int] -> ([Int],Int)
findIncreaseCycle totalSteps width lens = (reverse $ take cycleStart lens', increaseCycle)
  where lens' = [ n | (i,n) <- zip [0..] lens, (i `rem` width) == (totalSteps `rem` width) ]
        (cycleStart,increaseCycle) = first (+2) . findNoDiff . differences . differences $ lens'
        differences xs = [ b - a | [a,b] <- divvy 2 1 xs ]
        findNoDiff xs = head [ (i,a) | (i,[a,b]) <- zip [0..] $ divvy 2 1 xs, a == b ]
        
step::Set Pos -> Set Pos -> Set Pos
step rocks visited = S.unions (S.map adjacent visited) S.\\ rocks

step2::Int -> Set Pos -> Set Pos -> Set Pos
step2 width rocks = S.filter isGarden . S.unions . S.map adjacent
  where isGarden = (`S.notMember` rocks) . both (translate . (`rem` width))  
        translate n | n < 0     = width + n
                    | otherwise = n

adjacent::Pos -> Set Pos
adjacent pos = S.fromList $ [first,second] <*> [pred,succ] <*> pure pos
