{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Aoc07 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (group, sort, sortOn, sortBy, partition)
import Data.Char (isDigit, digitToInt)
import Data.Bifunctor (bimap, second)
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
solution part = sum . zipWith (*) [1..] . map bid . sortBy (comparing (handType part) <> compareCards part)
  where compareCards PartOne = comparing cards
        compareCards PartTwo = comparing $ map compareJoker . cards

handType::Part->Hand -> HandType
handType part Hand{..} = case groupLenghs part of 
          [5]     -> FiveOfAKind
          (4:_)   -> FourOfAKind
          [3,2]   -> FullHouse
          (3:_)   -> ThreeOfAKind
          [2,2,1] -> TwoPair
          2:_     -> OnePair
          _       -> HighCard
  where groupLenghs PartOne = groupSorted cards
        groupLenghs PartTwo = case groupSorted cards' of
                n:rest -> n + numJokers : rest
                []     -> [5]
        (cards', numJokers) = second length . partition (/= CL J) $ cards
        groupSorted = sortOn negate . map length . group . sort

compareJoker::Card -> Int
compareJoker = \case 
        CL J -> 0
        CN n -> n
        CL l -> fromEnum l + 10