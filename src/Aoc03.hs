module Aoc03 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (groupOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Char (isDigit,digitToInt)
import Data.Tuple.Extra ((&&&))

type Pos  = (Int,Int)
type Number = (Pos,Int)
type NumberAdj = (Set Pos,Int)
type Symbol = (Pos,Char)

parseInput::Text -> ([Number],[Symbol])
parseInput txt = (concatMap parseLineNumbers . zip [0..] . T.lines $ txt,symbols)
  where parseLineNumbers (j,line) = [((i,j), parseDigits nDigits) | nDigits@((i,c):_) <- numberGroups line, isDigit c]
        numberGroups line = groupOn (isDigit . snd) . zip [0..] . T.unpack $ line
        parseDigits = toDecimal . map (digitToInt . snd)
        toDecimal = foldl (\ acc b -> acc * 10 + b) 0 
        symbols = [((i,j),c) | (j,line) <- zip [0..] . T.lines $ txt, (i,c)<- zip [0..] . T.unpack $ line, c /='.' && not (isDigit c)]

solution::Part -> ([Number],[Symbol]) -> Int
solution part (numbers, symbols) = case part of 
    PartOne -> sum . map snd . filter (adjacentToSymbol symbolsSet) $ numbersAdj
    PartTwo -> sum . S.map (adjacentToGear numbersAdj) $ gearPositions
  where numbersAdj = map (adjacentPositions &&& snd) numbers
        symbolsSet = S.fromList . map fst $ symbols
        gearPositions = S.fromList [pos | (pos,'*') <- symbols]

adjacentToSymbol:: Set Pos -> NumberAdj -> Bool
adjacentToSymbol symbols = not . null . S.intersection symbols . fst
        
adjacentToGear::[NumberAdj] -> Pos -> Int
adjacentToGear numbers gearPos = case filter (S.member gearPos . fst) numbers of
        [(_,n1),(_,n2)] -> n1 * n2
        _               -> 0

adjacentPositions::Number->Set Pos
adjacentPositions ((i,j),number) = S.fromList $ (i-1,j) : (i+numDigits,j) : ((,) <$> [i-1 .. i + numDigits] <*> [j-1,j+1])
  where numDigits = length . show $ number
