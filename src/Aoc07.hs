{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Aoc07 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (group, sort, sortOn, sortBy, partition, uncons)
import Data.Char (isDigit, digitToInt)
import Data.Bifunctor (bimap, first)
import Data.Ord (comparing)

data CardL = T | J | Q | K | A deriving (Read,Show,Eq,Ord,Enum)
data Card = CN Int | CL CardL deriving (Show,Eq,Ord)
data Hand = Hand {cards::[Card], bid::Int} deriving Show
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq,Ord)

parseInput::Text -> [Hand]
parseInput = map (uncurry Hand . bimap (map parseCard . T.unpack) (read . T.unpack) . T.breakOn " ") . T.lines
  where parseCard c | isDigit c = CN $ digitToInt c
                    | otherwise = CL $ read [c]

solution::Part -> [Hand] -> Int
solution part = sum . zipWith (*) [1..] . map bid . sortBy (comparing (handType part) <> compareCards)
  where compareCards  = case part of
          PartOne -> comparing cards
          PartTwo -> comparing $ map cardPoints . cards

handType::Part -> Hand -> HandType
handType part Hand{..} = case groupLengths part of
          5:_   -> FiveOfAKind
          4:_   -> FourOfAKind
          3:2:_ -> FullHouse
          3:_   -> ThreeOfAKind
          2:2:_ -> TwoPair
          2:_   -> OnePair
          _     -> HighCard
  where groupLengths PartOne = groupSorted cards
        groupLengths PartTwo = maybe [5] (uncurry (:) . first (+ numJokers)) .  uncons . groupSorted $ cards'
        (cards', numJokers) = length <$> partition (/= CL J) cards
        groupSorted = sortOn negate . map length . group . sort

cardPoints::Card -> Int
cardPoints = \case
        CL J -> 0
        CL l -> fromEnum l + 10
        CN n -> n
