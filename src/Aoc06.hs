{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc06 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.Tuple.Extra (both)

data Race = Race{ time::Int, distance::Int} deriving Show

parseInput::Text -> ([Text],[Text])
parseInput = both (tail . T.words) . T.breakOn "\n"

solution::Part -> ([Text],[Text]) -> Int
solution PartOne = product . map numWaysToWin . races
  where races = uncurry (zipWith Race) . both (map (read . T.unpack))
solution PartTwo = numWaysToWin . race
  where race = uncurry Race . both (read . T.unpack . T.concat)

numWaysToWin::Race -> Int
numWaysToWin Race{..} = length . filter (> distance) . map timeDistance $ [1..time]
  where timeDistance pushTime = (time - pushTime) * pushTime 
