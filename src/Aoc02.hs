{-# LANGUAGE OverloadedStrings #-}

module Aoc02 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor (bimap)
import Data.Tuple.Extra (swap)

data Colour = Blue | Green | Red deriving (Show,Read,Eq,Ord)
type Turn = [(Colour,Int)]
data Game = Game {gid::Int,turns::[Turn]} deriving Show

parseInput::Text -> [Game]
parseInput = map (uncurry Game . bimap (read . T.unpack . (!!1) . T.words) (map parseTurn . T.splitOn "; " . T.drop 2) . T.breakOn ":") . T.lines
  where parseTurn = map (bimap (read . T.unpack . T.toTitle . T.tail) (read . T.unpack) . swap . T.breakOn " ") . T.splitOn ", "

solution::Part -> [Game] -> Int
solution PartOne = sum . map gid . filter (isValid maxGame)
  where maxGame = M.fromList [(Red,12),(Green,13),(Blue,14)]
solution PartTwo = sum . map power

isValid::Map Colour Int -> Game -> Bool
isValid maxGame = all (all validTurn) . turns
  where validTurn (colour,n) = M.findWithDefault 0 colour maxGame >= n
  
power::Game -> Int
power = product . M.elems . foldl maxCubes M.empty . concat . turns
  where maxCubes acc (colour,n) = M.insertWith max colour n acc  