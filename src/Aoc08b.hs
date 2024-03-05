{-# LANGUAGE OverloadedStrings #-}

module Aoc08b (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (findIndex, elemIndex)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor (bimap)

data Dir = L | R deriving (Read,Show)
type Network = Map Text (Text,Text)

parseInput::Text -> ([Dir],Network)
parseInput = bimap (map (read . (:[])) . T.unpack) (M.fromList . parseNodes) . T.breakOn "\n\n"
  where parseNodes lns = [(a,(b,c)) | [a,_,_,_,b,_,c,_] <- map (T.split (`T.elem` "=,() ")) . T.lines $ lns]

solution::Part -> ([Dir],Network) -> Maybe Int
solution part (directions,network) = case part of
      PartOne -> elemIndex "ZZZ" . applyMovements $ "AAA"
      PartTwo -> foldl1 lcm <$> traverse (findIndex endsInZ . applyMovements) initNodes
  where applyMovements start = scanl (move network) start . cycle $ directions  
        endsInZ = T.isSuffixOf "Z"
        initNodes = filter (T.isSuffixOf "A") . M.keys $ network

move::Network -> Text -> Dir -> Text
move network location dir = move' dir $ network M.! location
  where move' L = fst
        move' R = snd
        