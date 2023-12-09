module Aoc09 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T

parseInput::Text -> [[Int]]
parseInput = map (map (read . T.unpack) . T.words) . T.lines

solution::Part -> [[Int]] -> Int
solution part = sum . map (nextValue part)
  where nextValue PartOne = sum . completeNumbers (-) . reverse
        nextValue PartTwo = foldr1 (-) . completeNumbers (flip (-))

completeNumbers::(Int ->Int -> Int) -> [Int] -> [Int]
completeNumbers op series
      | all (==0) differences = take 1 series
      | otherwise             = head series : completeNumbers op differences
  where differences = zipWith op series $ tail series