{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Aoc23 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.Matrix (Matrix, nrows, ncols)
import qualified Data.Matrix as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromJust, catMaybes, maybeToList)
import Data.Either.Extra (mapRight, fromEither, partitionEithers)
import Data.Bifunctor (first,second)
import Data.Tuple.Extra (secondM, (&&&))

data Dir = N | E | S | W deriving (Show, Eq)
data Cell = Empty | Forest | Slope Dir deriving (Show,Eq)
data You = You { pos :: Pos, path :: Set Pos}
data MovementRes = Blocked Int | Continue You | SlopeReached You

type World = Matrix Cell
type Pos = (Int,Int)
type Graph = Map Pos [(Pos,Int)]

parseInput::Text -> World
parseInput = M.fromLists . map (map toCell . T.unpack) . T.lines
  where toCell c = case c of
          '>' -> Slope E
          '^' -> Slope N
          'v' -> Slope S
          '<' -> Slope W
          '#' -> Forest
          _   -> Empty

solution::Part -> World -> Int
solution PartOne world = findLongestPath $ You (1,2) S.empty
  where findLongestPath you = fromEither . mapRight (maximum . map findLongestPath) $ reachNextSlope world you
solution PartTwo world = fromJust $ dfs graph (nrows world, ncols world - 1) ((1,2),0)
  where graph = createGraph $ removeSlope <$> world
        removeSlope (Slope _) = Empty
        removeSlope cell      = cell

reachNextSlope::World -> You -> Either Int [You]
reachNextSlope world you | null slopeReached = Left $ maximum blocked
                         | otherwise         = Right $ concat slopeReached 
  where (blocked, slopeReached) = partitionEithers . map next $ move world you
        next (Blocked n)         = Left n
        next (Continue you')     = reachNextSlope world you'
        next (SlopeReached you') = Right [you']

move:: World -> You -> [MovementRes]
move world You{..} = map moveToCell adjacentCells
  where moveToCell = \case
            (_, (pos', Empty))
                  | pos' `S.member` path  -> Blocked (S.size path)
                  | otherwise             -> Continue $ You pos' (S.insert pos' path)
            (_, (_, Forest))              -> Blocked (S.size path)
            (dir, (pos', Slope slDir))
                  | dir == slDir          -> SlopeReached $ You (move' dir pos') (S.fromList [pos', move' dir pos'] `S.union` path)
                  | otherwise             -> Blocked (S.size path)
        adjacentCells = mapMaybe (secondM (\(y,x) -> ((y,x), ) <$> M.safeGet y x world)) . zip [N,S,W,E] $ adjacents pos
        move' N = first pred
        move' E = second succ
        move' S = first succ
        move' W = second pred

createGraph::World -> Graph
createGraph world = Map.fromList . map (id &&& vertexNeighbours world vertices) $ vertices
  where vertices = [(1,2),(nrows world,ncols world - 1)] ++ (catMaybes . M.toList . M.mapPos mapV $ world)
        mapV pos Empty | isVertex pos = Just pos
        mapV _ _                      = Nothing
        isVertex = (>=3) . length . filter ((/=Forest) . snd ) . mapMaybe (\(y,x) -> ((y,x),) <$> M.safeGet y x world) . adjacents

vertexNeighbours::World -> [Pos] -> Pos -> [(Pos, Int)]
vertexNeighbours world vertices pos' = concatMap (findNeighbour 0) paths
   where paths = [ you | (Continue you) <- move world $ You pos' (S.singleton pos')]
         findNeighbour cnt you@You{..}
              | pos `elem` vertices = [(pos, cnt+1)]
              | otherwise           = findNeighbour (cnt+1) $ head [ you' | (Continue you') <- move world you]

dfs::Graph -> Pos -> (Pos,Int) -> Maybe Int
dfs graph targetPos (pos,cost) | pos == targetPos = Just cost
                               | null subMaximums = Nothing
                               | otherwise        = Just $ maximum subMaximums
  where subMaximums = maybeToList (graph !? pos) >>= mapMaybe next
        next (pos',cost') = dfs (Map.delete pos graph) targetPos (pos',cost + cost')

adjacents::Pos -> [Pos]
adjacents pos= [first,second] <*> [pred,succ] <*> pure pos