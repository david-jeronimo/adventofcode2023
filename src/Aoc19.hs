{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Aoc19 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Split (chunksOf)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)
import Data.Monoid (First(..))

data Prop     = X | M | A | S deriving (Show, Read, Eq, Ord, Enum)
data Result   = Accepted | Rejected deriving (Show, Eq)
data Target   = TR Result | TW Text deriving (Show, Eq)
data Rule     = Rule { prop::Prop, cmp::Ordering, val::Int, target::Target } deriving Show
data Workflow = Workflow { rules:: [Rule], fallback::Target} deriving Show

type PR     = Map Prop Int
type Rng    = Map Prop (Int,Int)
type MinMax = (Int,Int)

parseInput::Text -> (Map Text Workflow, [PR])
parseInput = bimap (M.fromList . parseWorkflows) (map parsePart . drop 2 . T.lines) . T.breakOn "\n\n"
  where parseWorkflows txt = [(wn,parseWorkflow rls) | wn:rls <- map (init . T.split (`T.elem` ":,{}")) . T.lines $ txt ]
        parseWorkflow rls = Workflow (map (parseRule . map T.unpack) . chunksOf 2 . init $ rls) (parseTarget . T.unpack . last $ rls)
        parseRule [p:cmp:v,tgt] = Rule (read [toUpper p]) (parseCmp cmp) (read v) (parseTarget tgt)
        parseRule _               = error "Invalid rule"
        parseCmp '>' = GT
        parseCmp _   = LT
        parseTarget "A" = TR Accepted
        parseTarget "R" = TR Rejected
        parseTarget tgt = TW . T.pack $ tgt
        parsePart = M.fromList . map (bimap (read . T.unpack . T.toTitle) (read . T.unpack . T.tail) . T.breakOn "=") . T.splitOn "," . T.init . T.tail

solution::Part -> (Map Text Workflow, [PR]) -> Int
solution part (workflows, parts) = case part of 
      PartOne -> sum . map points . filter ((== Accepted) . processPart workflows "in") $ parts
      PartTwo -> sum . map sumCombinations $ combinations workflows ("in", initRange)
  where points = sum . M.elems
        initRange = M.fromList . map (,(1,4000)) . enumFrom $ X
        sumCombinations = product . map (\(from,to) -> to - from + 1) . M.elems 

processPart::Map Text Workflow -> Text -> PR -> Result
processPart workflows wfTitle pr = case applyWorkflow (workflows!wfTitle) pr of
      TR result -> result
      TW nextWf -> processPart workflows nextWf pr
  where applyWorkflow Workflow{..} parts = fromMaybe fallback . getFirst . foldMap (First . applyRule parts) $ rules

applyRule::PR -> Rule -> Maybe Target
applyRule part Rule{..} | success   = Just target
                        | otherwise = Nothing
  where success = compare (part ! prop) val == cmp

combinations::Map Text Workflow -> (Text,Rng) -> [Rng]
combinations workflows (wfName, range) = concatMap progress $ validRanges workflows (wfName, range)
  where progress (_,   TR Rejected) = []
        progress (rgn, TR Accepted) = [rgn]
        progress (rgn, TW wfName')  = combinations workflows (wfName',rgn)

validRanges::Map Text Workflow -> (Text,Rng) -> [(Rng, Target)]
validRanges workflows (wfName, range) = applyRules rules range
  where Workflow{..} = workflows!wfName
        applyRules (rule:rules') range' = positiveRule range' rule : applyRules rules' (negativeRule range' rule)
        applyRules []            range' = [(range', fallback)]
        positiveRule range' rule@Rule{..} = (applyConstraint range' prop (rangesForRule True rule), target)   
        negativeRule range' rule@Rule{..} = applyConstraint range' prop . rangesForRule False $ rule

applyConstraint::Rng -> Prop -> MinMax -> Rng
applyConstraint rng p (a,b) = updateRangeEntry $ rng ! p 
  where updateRangeEntry (a1,b1) = M.insert p (max a1 a, min b1 b) rng

rangesForRule::Bool -> Rule -> MinMax
rangesForRule success Rule{..} = case (success,cmp) of
    (True,  LT) -> (minBound, val - 1)
    (True,  GT) -> (val + 1, maxBound)
    (False, LT) -> (val, maxBound)
    _           -> (minBound, val)
