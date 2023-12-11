{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Aoc11 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (tails, sortOn)
import Data.Bifunctor (first, second)

type Galaxy = (Int,Int)
data Dim = X | Y

parseInput::Text -> [Galaxy]
parseInput txt = [(i,j) | (j,line) <- zip [0..] $ T.lines txt, (i,'#') <- zip [0..] $ T.unpack line]

solution::Part -> [Galaxy] -> Int
solution = \case 
    PartOne -> distances . expand 1
    PartTwo -> distances . expand (1_000_000 - 1)
  where expand n = expandDim n Y . expandDim n X

distances::[Galaxy] -> Int
distances galaxies = sum [dist g1 g2 | (g1:gs) <- tails galaxies, g2 <- gs]
  where dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

expandDim::Int -> Dim -> [Galaxy] -> [Galaxy]
expandDim units dim galaxies = expandDim' 0 . zip galaxiesByDim $ tail galaxiesByDim ++ [head galaxiesByDim]
  where galaxiesByDim = sortOn dimF galaxies
        (dimF,f) = case dim of
            X -> (fst,first)
            Y -> (snd,second)
        expandDim' _ [] = []
        expandDim' offset ((g1,g2):galaxies') = f (+ offset) g1 : expandDim' offset' galaxies'
            where offset' | dimF g1 == dimF g2 = offset
                          | otherwise          = offset + units * (dimF g2 - dimF g1 - 1)                                                                                                                                                      
        