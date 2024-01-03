{-# LANGUAGE OverloadedStrings #-}

module Aoc22 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (delete, sortOn, (\\), unfoldr)
import Data.Maybe (mapMaybe)
import Safe.Foldable (maximumDef)
import Control.Applicative (liftA2)

type Rng = (Int,Int)
data Brick = Brick {x::Rng, y::Rng, z::Rng} deriving (Show,Eq)

parseInput::Text -> [Brick]
parseInput txt = [Brick (x1,x2) (y1,y2) (z1,z2) | [x1,y1,z1,x2,y2,z2] <- map (map (read . T.unpack) . T.split (`T.elem` ",~")) . T.lines $ txt]

solution::Part -> [Brick] -> Int
solution part = f . settleBricks
  where f bricks = case part of
          PartOne -> length . filter (canBeDisintegrated bricks) $ bricks
          PartTwo -> sum . map numDisintegrated $ bricks
            where numDisintegrated brick = sum $ unfoldr crumble (bricks, [brick])

settleBricks::[Brick] -> [Brick]
settleBricks = foldl fall [] . sortOn (fst . z)
  where fall settled = (:settled) . settleBrick settled

settleBrick::[Brick] -> Brick -> Brick
settleBrick settledBricks fallingBrick@(Brick _ _ (z1a,z1b)) = fallingBrick{z = (maxZ + 1, maxZ + z1b - z1a + 1)}
    where maxZ = maximumDef 0 . mapMaybe zIfBelow $ settledBricks 
          zIfBelow brick@(Brick _ _ (_,zb)) | brick `isBelow` fallingBrick = Just zb
          zIfBelow _                                                       = Nothing

crumble::([Brick],[Brick]) -> Maybe (Int, ([Brick],[Brick]))
crumble (bricks, removed) | null disintegrated = Nothing
                          | otherwise          = Just (length disintegrated, (remaining, disintegrated))
  where bricksAbove            = filter (or . liftA2 isDirectlyBelow removed . pure) bricks
        isSupported brickAbove = any (`isDirectlyBelow` brickAbove) remaining
        disintegrated          = filter (not . isSupported) bricksAbove
        remaining              = bricks \\ removed

canBeDisintegrated::[Brick] -> Brick -> Bool
canBeDisintegrated bricks brick = null bricksAbove || all isSupported bricksAbove
  where isSupported brickAbove = any (`isDirectlyBelow` brickAbove) (delete brick bricks)
        bricksAbove = filter (isDirectlyBelow brick) . delete brick $ bricks

isBelow::Brick -> Brick -> Bool
isBelow (Brick x1 y1 (_,z1b)) (Brick x2 y2 (z2a,_)) = (z1b < z2a) && not (disjoint x1 x2) && not (disjoint y1 y2)
  where disjoint (a1,b1) (a2,b2) = b1 < a2 || b2 < a1 

isDirectlyBelow::Brick -> Brick -> Bool
isDirectlyBelow brick1@Brick{z=(_,z1b)} brick2@Brick{z=(z2a,_)} = z2a == z1b + 1 && brick1 `isBelow` brick2