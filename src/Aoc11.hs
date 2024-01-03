{-# LANGUAGE NumericUnderscores #-}

module Aoc11 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (tails, sortOn, unfoldr, snoc)
import Data.Bifunctor (first, second)

type Galaxy = (Int,Int)
data Dim = X | Y

parseInput::Text -> [Galaxy]
parseInput txt = [(i,j) | (j,line) <- zip [0..] $ T.lines txt, (i,'#') <- zip [0..] $ T.unpack line]

solution::Part -> [Galaxy] -> Int
solution part = distances . expandDim n Y . expandDim n X
  where n = case part of
          PartOne -> 1
          PartTwo -> 1_000_000 - 1

distances::[Galaxy] -> Int
distances galaxies = sum [ dist g1 g2 | (g1:gs) <- tails galaxies, g2 <- gs ]
  where dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

expandDim::Int -> Dim -> [Galaxy] -> [Galaxy]
expandDim units dim galaxies = unfoldr expandDim' (snoc sortedGalaxies $ head sortedGalaxies, 0)
  where sortedGalaxies = sortOn dimF galaxies
        expandDim' (g1:g2:galaxies', offset) = Just (f (+offset) g1, (g2:galaxies', offset'))
            where offset' | dimF g1 == dimF g2 = offset
                          | otherwise          = offset + units * (dimF g2 - dimF g1 - 1)
        expandDim' _                         = Nothing
        (dimF,f) = case dim of
            X -> (fst,first)
            Y -> (snd,second)