{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc15 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (sortOn, groupSortOn, groupOnKey)
import Data.Maybe (fromJust, isJust)
import Data.Char (isLetter)
import Data.Tuple.Extra ((&&&), second)

data Operation = Op { lens :: Text, value :: Maybe Int, idx :: Int } deriving Show
type BoxId = Int

instance Semigroup Operation
  where op1@(Op _ (Just _) _) <> (Op _ (Just _) id2) = op1{idx=id2}
        op1                   <> _                   = op1

instance Monoid Operation where mempty = Op T.empty Nothing 0

parseInput::Text -> ([Text],[Operation])
parseInput = (id &&& zipWith (curry parseStep) [0..]) . T.splitOn ","
  where parseStep (index, step) = case second T.unpack . T.span isLetter $ step of
          (lens','=':rest) -> Op lens' (Just $ read rest) index
          (lens',_)        -> Op lens' Nothing index

solution::Part -> ([Text],[Operation]) -> Int
solution PartOne = sum . map hash . fst
solution PartTwo = sum . map (uncurry boxPower) . reduceAndGroupByBox . snd

boxPower::BoxId -> [Operation] -> Int
boxPower boxId = sum . map focusingPower . zipWith setRelativeIndex [1..] . sortOn idx
  where setRelativeIndex i op = op{idx=i}
        focusingPower Op{..} = (boxId + 1) * fromJust value * idx

reduceAndGroupByBox::[Operation] -> [(BoxId, [Operation])]
reduceAndGroupByBox = groupOnKey (hash . lens) . sortOn (hash . lens) . reduceOperations
  where reduceOperations = filter (isJust . value) . map mconcat . groupSortOn lens . reverse 

hash::Text -> BoxId
hash = foldl hash' 0 . map fromEnum . T.unpack
  where hash' acc v = ((acc + v) * 17) `rem` 256
