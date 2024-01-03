{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Aoc12 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor (bimap)
import Control.Monad.State (State, evalState, modify, get)

data Spring = Opr | Dmg | Unk deriving (Show,Eq)
type Row = ([Spring],[Int])
type App = State (Map (Int,Int,[Int]) Int)

parseInput::Text -> [Row]
parseInput = map (bimap (map toSpring . T.unpack) (map (read . T.unpack) . T.splitOn ",") . T.breakOn " ") . T.lines
  where toSpring = \case
            '.' -> Opr
            '#' -> Dmg
            _   -> Unk

solution::Part -> [Row] -> Int
solution = \case 
      PartOne -> sum . map numCombinations
      PartTwo -> sum . map (numCombinations . repeat5)      
  where numCombinations (springs, groups) = evalState (arrangements springs groups 0 0) M.empty
        repeat5 = bimap (intercalate [Unk] . replicate 5) (concat . replicate 5)  

arrangements::[Spring] -> [Int] -> Int -> Int -> App Int
arrangements []      []     _ _                           = return 1
arrangements springs []     _ _      | Dmg `elem` springs = return 0
arrangements _       []     _ _                           = return 1
arrangements []      [gr]   _ dmgCnt | dmgCnt == gr       = return 1
arrangements []      _      _ _                           = return 0
arrangements (spring:springs) (gr:groups) cnt dmgCnt = do
          case spring of
            Opr | dmgCnt == gr -> addOprComplete
            Opr | dmgCnt > 0   -> return 0
            Opr                -> addOpr
            Dmg | dmgCnt == gr -> return 0
            Dmg                -> addDmg
            Unk | dmgCnt == gr -> addOprComplete
            Unk | dmgCnt > 0   -> addDmg
            Unk                -> sum <$> sequence [addOpr, addDmg]
  where addDmg         = arrangementsCached springs (gr:groups) (cnt+1) (dmgCnt+1)
        addOpr         = arrangementsCached springs (gr:groups) (cnt+1) 0
        addOprComplete = arrangementsCached springs groups      (cnt+1) 0

arrangementsCached::[Spring] -> [Int] -> Int -> Int -> App Int
arrangementsCached springs groups cnt dmgCnt = do
  cache <- get
  case cache M.!? cacheKey of
    Just v  -> return v
    Nothing -> do
          v <- arrangements springs groups cnt dmgCnt
          modify $ M.insert cacheKey v
          return v
  where cacheKey = (cnt,dmgCnt,groups)       