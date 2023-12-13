{-# LANGUAGE OverloadedStrings #-}

module Aoc13 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (find)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bifunctor (second, bimap)
import Data.Tuple.Extra ((&&&), both, swap)
import Control.Applicative ((<|>))

type Pos = (Int,Int)
type Pattern = (Set Pos, Pos)

parseInput::Text -> [Pattern]
parseInput = map ((S.fromList &&& findMax) . parsePattern) . T.splitOn "\n\n"
  where parsePattern txt = [(i,j) | (j,line) <- zip [0..] $ T.lines txt, (i,'#') <- zip [0..] $ T.unpack line]
        findMax = both maximum . unzip

solution::Part -> [Pattern] -> Maybe Int
solution part = fmap sum . traverse (patternPoints part)

patternPoints::Part -> Pattern -> Maybe Int
patternPoints part pattern@(_, (maxX, maxY)) = pointsHorizontal <|> pointsVertical
  where pointsHorizontal = (*100) <$> find (foldH part pattern) [1..maxY]
        pointsVertical = find (foldH part transposedPattern) [1..maxX]
        transposedPattern = bimap (S.map swap) swap pattern

foldH::Part -> Pattern -> Int -> Bool
foldH part (pattern, (_, maxY)) mirrorY = case part of
      PartOne -> reflection' == reflection
      PartTwo -> ((==1) . S.size . S.unions $ [(S.\\), flip (S.\\)] <*> pure reflection <*> pure reflection')
                 && (elem (S.size (reflection `S.intersection` reflection')) . map S.size $ [orig, reflection])
  where (reflection,orig) = S.partition ((>= mirrorY) . snd) . S.filter inMirror $ pattern
        reflection' = (S.map (second (\y -> 2 * mirrorY - y - 1)) orig)
        inMirror (_,y) = y >= (mirrorY - reflectionSize) && y < (mirrorY + reflectionSize)
        reflectionSize = min mirrorY (maxY - mirrorY + 1)
