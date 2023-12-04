{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Aoc04 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (intersect)

data Card = Card{winning::[Int], owned::[Int]} deriving Show

parseInput::Text -> [Card]
parseInput txt = [Card (parse win) (parse own) | [_,win,own] <- map (T.split (`T.elem` "|:")) . T.lines $ txt]
  where parse = map (read . T.unpack) . T.words

solution::Part -> [Card] -> Int
solution  = \case
      PartOne -> sum . map cardPoints
      PartTwo -> sum . foldr (accumCards . numMatches) []
  where cardPoints card = case numMatches card of
             0 -> 0
             n -> 2^(n-1)
        numMatches Card{..} = length $ winning `intersect` owned

accumCards::Int -> [Int] -> [Int]
accumCards n rest = 1 + (sum . take n $ rest) : rest
