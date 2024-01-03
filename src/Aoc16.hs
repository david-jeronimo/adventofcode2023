{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Aoc16 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (find)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Bifunctor (first, second)
import Data.Tuple.Extra ((&&&))

data Dir = N | E | S | W deriving (Show,Eq,Ord)
data MirrorT = ES | EN deriving Show
data SplitterT = V | H deriving Show
data Cell = Mirror MirrorT | Splitter SplitterT deriving Show
type Pos = (Int,Int)
type MaxXY = Pos
type World = Map Pos Cell
type Beam = (Pos,Dir)

parseInput::Text -> (World, MaxXY)
parseInput txt = (parseWorld, ((pred . T.length . head) &&& pred . length) . T.lines $ txt)
  where parseWorld = M.fromList [((i,j),parseCell c) | (j,line) <- zip [0..] $ T.lines txt, (i,c) <- zip [0..] $ T.unpack line, c/= '.']
        parseCell = \case
            '/'  -> Mirror EN
            '\\' -> Mirror ES
            '|'  -> Splitter V
            '-'  -> Splitter H
            _    -> error "Invalid input"

solution::Part -> (World, MaxXY) -> Int
solution part (world, maxXY@(maxX,maxY)) = case part of
      PartOne -> energizedTiles ((0,0),E)
      PartTwo -> maximum . map energizedTiles $ startTiles
  where energizedTiles beam@(pos,_) = S.size . S.insert pos . S.map fst . visitedPositions $ (S.singleton beam, S.empty)
        visitedPositions = snd . fromJust . find (null . fst) . iterate (beamSteps world maxXY)
        startTiles = concat [map ((,S) . (,0))    [0..maxX],
                             map ((,N) . (,maxY)) [0..maxX],
                             map ((,E) . (0,))    [0..maxY],
                             map ((,W) . (maxX,)) [0..maxY]]

beamSteps::World -> MaxXY -> (Set Beam, Set Beam) -> (Set Beam, Set Beam)
beamSteps world (maxX,maxY) (beams, visited) = (beams' S.\\ visited, visited `S.union` beams')
  where beams' = S.fromList . filter bounded . concatMap (moveBeam world) $ beams
        bounded ((x,y),_) = x >= 0 && x <= maxX && y >= 0 && y <= maxY

moveBeam::World -> Beam -> [Beam]
moveBeam world (pos,dir) = case world M.!? pos of
                  Nothing          -> [move dir]
                  Just (Mirror EN) -> case dir of
                      N -> [move E]
                      E -> [move N]
                      S -> [move W]
                      W -> [move S]
                  Just (Mirror ES) -> case dir of
                      N -> [move W]
                      E -> [move S]
                      S -> [move E]
                      W -> [move N]
                  Just (Splitter V)
                      | dir `elem` [N,S] -> [move dir]
                      | otherwise        -> [move N, move S]
                  Just (Splitter H)
                      | dir `elem` [E,W] -> [move dir]
                      | otherwise        -> [move E, move W]
  where move dir' = (move' dir' pos, dir')
        move' N = second pred
        move' E = first succ
        move' S = second succ
        move' W = first pred
