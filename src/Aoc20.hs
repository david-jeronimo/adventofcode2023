{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Aoc20 (parseInput, solution) where

import Lib (Part(..), Text)
import qualified Data.Text as T
import Data.List.Extra (partition)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Tuple.Extra (fst3, snd3, thd3, both, second, first3, second3, third3)
import Control.Monad.State (State, execState, evalState, gets, modify)
import Control.Monad (replicateM_)

data    Signal      = High | Low deriving (Show,Eq)
newtype FlipFlop    = FlipFlop {on :: Bool} deriving Show
newtype Conjunction = Conjunction {inputPulses :: Map Text Signal} deriving Show
data ModuleT        = Broadcaster | F FlipFlop | C Conjunction deriving Show
data Module         = Module {mdl::ModuleT, destinations::[Text] } deriving Show
data Counter        = Counter {cntHigh::Int, cntLow::Int}
type St             = (Map Text Module, Counter, Bool)
type App            = State St

parseInput::Text -> Map Text Module
parseInput = M.fromList . updateConjunctions . map (parseModule . second (T.drop 4) . T.breakOn " -> ") . T.lines
  where updateConjunctions modules = map updateConj modules
          where updateConj (cid, m@Module{mdl=(C (Conjunction _))}) = (cid, m{mdl=C $ Conjunction (inputNames cid)})
                updateConj other = other
                inputNames cid = M.fromList . map ((,Low) . fst) . filter (elem cid . destinations . snd) $ modules 

parseModule::(Text,Text) -> (Text,Module)
parseModule (mid, dest) = case T.unpack mid of
    "broadcaster" -> (mid, Module Broadcaster parseDest)
    '%':mid'      -> (T.pack mid', Module (F $ FlipFlop False) parseDest)
    '&':mid'      -> (T.pack mid', Module (C $ Conjunction M.empty) parseDest)
    _             -> error "Invalid input"
  where parseDest = T.splitOn ", " dest   

solution::Part -> Map Text Module -> Int
solution part modules = case part of
    PartOne -> countHighLows . execState (replicateM_ 1000 (increaseCounter (0,1) >> pushButton T.empty)) $ initState
    PartTwo -> foldl1 lcm . map iterationsUntilHighSent $ ["pv","qh","xm","hz"]
  where countHighLows (_, Counter{..},_) = cntHigh * cntLow
        initState = (modules, Counter 0 0, False)
        iterationsUntilHighSent mid = evalState (pushUntilDone mid 0) initState

pushButton::Text -> App ()
pushButton senderTracked = signalCycle senderTracked [(T.empty,"broadcaster",Low)]

pushUntilDone::Text -> Int -> App Int
pushUntilDone senderTracked cnt = do 
        highSent' <- gets thd3
        if highSent' then return cnt
        else pushButton senderTracked >> pushUntilDone senderTracked (cnt+1)

signalCycle::Text -> [(Text,Text,Signal)] -> App ()
signalCycle senderTracked signals = do
        signals' <- concat <$> traverse (signalStep senderTracked) signals
        increaseCounter . both length . partition ((==High) . thd3) $ signals'
        if null signals' then return ()
        else signalCycle senderTracked signals'

signalStep::Text -> (Text,Text,Signal) -> App [(Text,Text,Signal)]
signalStep senderTracked (fromId,mid,signal) = do
      mods <- gets fst3
      case mods !? mid of
        Nothing           -> return [] 
        Just m@Module{..} -> case (mdl, signal) of
            (Broadcaster,_)               -> send signal m
            (F _, High)                   -> return []
            (F f@FlipFlop{on=False}, Low) -> updateModule mid (F f{on=True}) >> send High m
            (F f@FlipFlop{on=True},  Low) -> updateModule mid (F f{on=False}) >> send Low m
            (C Conjunction{..}, _)        -> do
                      let inputPulses' = M.insert fromId signal inputPulses
                      updateModule mid (C Conjunction{inputPulses=inputPulses'})
                      if all (== High) . M.elems $ inputPulses' 
                          then send Low m
                          else if mid == senderTracked 
                              then (modify . third3 . const $ True) >> send High m
                              else send High m    
  where send signal' = return . map (mid, ,signal') . destinations                         

increaseCounter :: (Int,Int) -> App ()
increaseCounter (numHigh, numLow) = do
    cnt'@Counter{..} <- gets snd3
    modify . second3 . const $ cnt'{cntHigh = cntHigh + numHigh, cntLow = cntLow + numLow} 

updateModule :: Text -> ModuleT -> App ()
updateModule mid m = do
    mods <- gets fst3
    modify . first3 . const $ M.update updateMod mid mods
  where updateMod modul@Module{} = Just modul{mdl=m}    
