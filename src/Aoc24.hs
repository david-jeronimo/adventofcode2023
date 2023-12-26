{-# LANGUAGE OverloadedStrings #-}

module Aoc24 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (tails)
import Data.Tuple.Extra (both, second)

data Hailstone = HS { pos::Pos, v::Pos } deriving (Show, Eq)
data Dim = X | Y | Z deriving Show

type Pos = (Int,Int,Int)

parseInput::Text -> [Hailstone]
parseInput txt = [HS (x,y,z) (vx,vy,vz) | ([x,y,z],[vx,vy,vz]) <- map (toPos . second T.tail . T.breakOn "@") . T.lines $ txt]
  where toPos = both (map (read . T.unpack) . T.splitOn ",")

solution::Part -> [Hailstone] -> String
solution PartOne hailstones = show $ length [(hs1,hs2) | (hs1:hss) <- tails hailstones, hs2 <- hss, intersectInTarget hs1 hs2]
  where target = (200000000000000, 400000000000000)
        intersectInTarget hs1 hs2 = isInTarget ip && isValidIntersection hs1 ip && isValidIntersection hs2 ip
            where ip = intersectionPoint hs1 hs2
        isInTarget (x,y) = x >= fst target && x <= snd target && y >= fst target && y <= snd target

{-- Could not get Z3 working. This returns the equations that can be pasted in https://quickmath.com/webMathematica3/quickmath/equations/solve/advanced.jsp.
    The coordinates will show as X,Y,Z. Their sum is the solution
--}
solution PartTwo hailstones = T.unpack $ buildEquation (head hailstones) (hailstones!!1) (hailstones!!2)

isValidIntersection::Hailstone -> (Float,Float) -> Bool
isValidIntersection (HS (x,y,_) (vx,vy,_)) (xc,yc)
    | vx > 0 && fromIntegral x > xc = False
    | vx < 0 && fromIntegral x < xc = False
    | vy > 0 && fromIntegral y > yc = False
    | vy < 0 && fromIntegral y < yc = False
    | otherwise                     = True

intersectionPoint::Hailstone -> Hailstone -> (Float, Float)
intersectionPoint (HS (x1,y1,_) (vx1,vy1,_)) (HS (x2,y2,_) (vx2,vy2,_)) = (x,y)
  where m1 = (fromIntegral vy1 / fromIntegral vx1)::Float
        m2 = (fromIntegral vy2 / fromIntegral vx2)::Float
        b1 = fromIntegral y1 + (fromIntegral x1 * (-m1))
        b2 = fromIntegral y2 + (fromIntegral x2 * (-m2))
        x = ((b2 - b1) / (m1 - m2))::Float
        y = m1 * x + b1

buildEquation::Hailstone -> Hailstone -> Hailstone -> Text
buildEquation hs1 hs2 hs3 = T.unlines . map T.pack $ concat [eq "t" hs1, eq "u" hs2, eq "v" hs3]
  where eq c (HS (x,y,z) (vx,vy,vz)) = ["x + a*" <> c <> " + (" <> show (-vx) <> "*" <> c <> ") = " <> show x,
                                        "y + b*" <> c <> " + (" <> show (-vy) <> "*" <> c <> ") = " <> show y,
                                        "z + c*" <> c <> " + (" <> show (-vz) <> "*" <> c <> ") = " <> show z]

