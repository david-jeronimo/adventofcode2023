{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Aoc25 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (sortOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Bifunctor (bimap)
import Data.Monoid (First(..))

type Connections = Map Text [Text]

parseInput::Text -> Connections
parseInput txt = M.unionWith (++) connections . M.unionsWith (++) $ [M.singleton n [key] | (key,nodes) <- M.assocs connections, n <- nodes]
  where connections = M.fromList . map (bimap T.init T.words . T.breakOnEnd ":") . T.lines $ txt

solution::Part -> Connections -> Maybe Int
solution PartOne connections = uncurry (*) . groupSizes <$> firstGroupSize
  where firstGroupSize = S.size <$> findGroup connections S.empty (M.fromList [(head $ M.keys connections, 1)])
        groupSizes size1 = (size1, M.size connections - size1)
solution PartTwo _           = Nothing

findGroup::Connections -> Set Text -> Map Text Int -> Maybe (Set Text)
findGroup connections added candidates
    | S.size added == M.size connections = Nothing
    | sum (M.elems candidates) == 3      = Just added
    | otherwise                          = getFirst . foldMap (First . addToGroup) $ sortedCandidates
  where sortedCandidates = map fst . sortOn (negate . affinity connections added) . M.assocs $ candidates
        addToGroup candidate = findGroup connections (S.insert candidate added) nextCandidates
          where nextCandidates = M.unionWith (+) newCandidates $ M.delete candidate candidates
                newCandidates = M.fromList . map (,1) . filter (`S.notMember` added) $ connections ! candidate

affinity::Connections -> Set Text -> (Text,Int) -> Int
affinity connections added (candidate,times) = (times * length (added `S.intersection` candidates)) - length candidates
  where candidates = S.fromList $ connections ! candidate
