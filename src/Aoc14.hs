{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Aoc14 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (transpose, sort)
import Data.List.Split (split, oneOf, keepDelimsL, divvy, chunksOf)
import Data.Tuple.Extra (secondM)
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid (First(..))
import Control.Applicative (liftA2)

data Rock = Cube | Rounded | None deriving (Show, Eq, Ord)

parseInput::Text -> [[Rock]]
parseInput = map (map toRock . T.unpack) . T.lines
  where toRock '#' = Cube
        toRock 'O' = Rounded
        toRock _   = None

solution::Part -> [[Rock]] -> Int
solution part mat = case part of
      PartOne -> totalLoad . transpose . moveAll . transpose $ mat
      PartTwo -> cycl !! ((1_000_000_000 - cycleStart) `rem` length cycl)
  where totalLoad = sum . zipWith (*) [1..] . map cntRounded . reverse
        cntRounded = length . filter (== Rounded)
        (cycleStart, cycl) = findCycle . map totalLoad . iterate moveCycle $ mat

moveCycle::[[Rock]] -> [[Rock]]
moveCycle = map reverse . rotate . rotate . rotate . moveAll . transpose
  where rotate = moveAll . transpose . map reverse

moveAll::[[Rock]] -> [[Rock]]
moveAll = map (concatMap sort . split (keepDelimsL $ oneOf [Cube]))

findCycle::[Int]-> (Int,[Int])
findCycle = fromJust . getFirst . foldMap First . zipWith (curry (secondM segmentCycle)) [0..] . divvy 20 1
  where segmentCycle segment = listToMaybe [a | (a:b:_) <- liftA2 chunksOf [4..10] (pure segment), a == b]
