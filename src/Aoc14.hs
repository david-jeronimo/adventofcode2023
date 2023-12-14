{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module Aoc14 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (transpose)
import Data.List.Split (split, oneOf, keepDelimsL, divvy, chunksOf)
import Data.Tuple.Extra (secondM)
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid (First(..))
import Control.Applicative (liftA2)

data Rock = Cube | Rounded | None deriving (Show, Eq)

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
        (cycleStart, cycl) = findCycle . map totalLoad . iterate moveCycle $ mat

moveCycle::[[Rock]] -> [[Rock]]
moveCycle = map reverse . rotate . rotate . rotate . moveAll . transpose
  where rotate = moveAll . transpose . map reverse

moveAll::[[Rock]] -> [[Rock]]
moveAll = map (concatMap moveSegment . split (keepDelimsL $ oneOf [Cube]))

moveSegment::[Rock] -> [Rock]
moveSegment (Cube:rest) = Cube : replicate (cntRounded rest) Rounded ++ replicate (length rest - cntRounded rest) None
moveSegment cubes = replicate (cntRounded cubes) Rounded ++ replicate (length cubes - cntRounded cubes) None

cntRounded::[Rock] -> Int
cntRounded = length . filter (== Rounded)

findCycle::[Int]-> (Int,[Int])
findCycle = fromJust . getFirst . mconcat . zipWith (curry (secondM segmentCycle)) [0..] . divvy 20 1
  where segmentCycle segment = First . fmap fst $ listToMaybe [(a,b)|(a:b:_) <- liftA2 chunksOf [4..10] . pure $ segment, a == b]
