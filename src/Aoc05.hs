{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc05 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List (uncons)
import Data.List.Split (chunksOf)
import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)

data Mapping = Mapping{len::Int,source::Int,dest::Int} deriving Show
data Range = Range{from::Int,to::Int} deriving Show

parseInput::Text -> ([Int],[[Mapping]])
parseInput = bimap parseSeeds (map parseMappings) . fromJust . uncons . T.splitOn "\n\n"
  where parseSeeds = map r . tail . T.words
        parseMappings txt = [Mapping c b a | [a,b,c] <- map (map r . T.words) . tail . T.lines $ txt]
        r = read . T.unpack

solution::Part -> ([Int],[[Mapping]]) -> Int
solution part (seeds,mappings) = minimum . map from . locationRanges . rangeSeeds $ part
  where locationRanges ranges = foldl translateRanges ranges mappings
        translateRanges ranges mapping = concatMap (translateRange mapping) ranges
        rangeSeeds PartOne = [Range n n | n <- seeds]
        rangeSeeds PartTwo = [Range from (from + leng - 1) | [from, leng] <- chunksOf 2 seeds]

translateRange::[Mapping]->Range->[Range]
translateRange _ Range{..} | from > to                = []
translateRange [] range                               = [range]
translateRange (Mapping{..}:mappings) range@Range{..} = addOffset within : concatMap (translateRange mappings) [before,after]
  where (before,within,after) = (range{to = min to (source - 1)},
                                 range{from = max from source, to = min to (source + len - 1)},
                                 range{from = max from (source+len)})
        addOffset Range{from=from',to=to'} = Range (from' + dest - source) (to' + dest - source)
