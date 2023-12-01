{-# LANGUAGE OverloadedStrings #-}

module Aoc01 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List (find)
import Data.Char (isDigit, digitToInt)
import Data.Bifunctor (second)
import Control.Applicative ((<|>))

parseInput::Text -> [Text]
parseInput = T.lines

solution::Part -> [Text] -> Maybe Int
solution part = fmap sum . traverse calibrationValue
  where calibrationValue txt = sum <$> sequenceA ([firstNumber, lastNumber] <*> pure txt)
        firstNumber = fmap (*10) . findNumber part wordMap
        lastNumber  = findNumber part (map (second T.reverse) wordMap) . T.reverse
        wordMap = [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five"),(6,"six"),(7,"seven"),(8,"eight"),(9,"nine")]

findNumber::Part->[(Int,Text)]->Text -> Maybe Int
findNumber part wordMap txt | T.null txt = Nothing
                            | otherwise  = parseDigit <|> parseWord part <|> (findNumber part wordMap . T.tail $ txt)
  where parseDigit | isDigit . T.head $ txt = Just . digitToInt . T.head $ txt
                   | otherwise              = Nothing
        parseWord PartOne = Nothing
        parseWord PartTwo = fmap fst . find (flip T.isPrefixOf txt . snd) $ wordMap