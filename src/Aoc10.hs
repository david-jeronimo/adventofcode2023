{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Aoc10 (parseInput, solution) where

import Lib (Part(..), Text, innerPositions)
import qualified Data.Text as T
import Data.List.Extra (partition, unfoldr)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor (first, second)
import Data.Tuple.Extra (both)
import Control.Monad (join)

data Dir = N | E | S | W deriving (Show,Eq, Enum)
data Tile = Tile Dir Dir deriving (Show,Eq)
data Animal = Animal {pos::Pos, dir::Dir}
data PathRes = Blocked | Loop [Pos] | PathItem Pos
type Pos = (Int,Int)
type Pipes = Map Pos Tile

instance Semigroup PathRes
  where Blocked      <> _         = Blocked
        Loop path    <> _         = Loop path
        PathItem pos <> Loop path = Loop (pos:path)
        PathItem _   <> other     = other

instance Monoid PathRes where mempty = Blocked

parseInput::Text -> (Pipes, Pos)
parseInput txt = (M.fromList . map (second toTile) $ tiles, fst . head $ start)
  where (start, tiles) = partition ((=='S') . snd) allTiles
        allTiles = [((i,j), c) | (j,line) <- zip [0..] $ T.lines txt, (i, c) <- zip [0..] $ T.unpack line, c/= '.']
        toTile = \case
          '|' -> Tile N S
          '-' -> Tile E W
          'L' -> Tile N E
          'J' -> Tile N W
          '7' -> Tile S W
          'F' -> Tile S E
          _   -> error "Invalid tile"

solution::Part -> (Pipes, Pos) -> Int
solution part (pipes, start) = case part of
      PartOne -> length loopPath `div` 2
      PartTwo -> S.size . (S.\\ S.fromList loopPath) . zoomOut . innerPositions . zoomIn $ loopPath  
  where loopPath = start : head [res | Loop res <- [findPath $ Animal (move dir' start) dir' | dir' <- enumFrom N]]
        findPath = mconcat . unfoldr (followPath start pipes)
        zoomOut = S.map (both (`div` 2))

followPath::Pos -> Pipes -> Animal -> Maybe (PathRes, Animal)
followPath start pipes animal@Animal{..}
    | start == pos = Just (Loop [], animal)
    | otherwise    = pipes M.!? pos >>= next
  where next (Tile dir1 dir2)    
          | dir1 == opposite dir = moveAnimal dir2
          | dir1 == dir          = moveAnimal (opposite dir2)
          | dir2 == opposite dir = moveAnimal dir1
          | dir2 == dir          = moveAnimal (opposite dir1)
          | otherwise            = Nothing
        moveAnimal dir' = Just (PathItem pos, Animal (move dir' pos) dir')
        opposite dir' = toEnum ((fromEnum dir' + 2) `rem` 4)

move::Dir -> Pos -> Pos
move N = second pred
move E = first succ
move S = second succ
move W = first pred

zoomIn::[Pos] -> [Pos]
zoomIn loopPath = concat . zipWith zoomIn' (last loopPath : loopPath) $ loopPath
  where zoomIn' a b = both (*2) a : expand a b
        expand (x1,y1) (x2,y2)
          | x1 == x2 && y1 > y2 = [(x1*2, y1*2-1)]
          | x1 == x2            = [(x1*2, y1*2+1)]
          | y1 == y2 && x1 > x2 = [(x1*2-1, y1*2)]
          | otherwise           = [(x1*2+1, y1*2)]
