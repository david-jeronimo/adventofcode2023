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
import Control.Monad.State (State, execState, evalState, get, gets, modify)
import Control.Monad (replicateM_)

data    Signal      = High | Low deriving (Show,Eq)
newtype FlipFlop    = FlipFlop {on :: Bool} deriving Show
newtype Conjunction = Conjunction {inputPulses :: Map Text Signal} deriving Show
data ModuleT        = Broadcaster | F FlipFlop | C Conjunction deriving Show
data Module         = Module {mdl::ModuleT, destinations::[Text] } deriving Show
data Counter        = Counter {steps::Int, cntHigh::Int, cntLow::Int}
type App            = State (Map Text Module, Counter, Map Text (Maybe Int))

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
    PartOne -> countHighLows . execState (replicateM_ 1000 pushButton) $ initState
    PartTwo -> foldl1 lcm . evalState pushUntilDone $ initState
  where countHighLows (_, Counter{..},_) = cntHigh * cntLow
        initState = (modules, Counter 0 0 0, M.fromList $ ( , Nothing) <$> sendersToTrack)
        sendersToTrack = head [ M.keys $ inputPulses conj | Module (C conj) dest <- M.elems modules, "rx" `elem` dest]

pushButton:: App ()
pushButton = increaseCounter True (0,1) >> signalCycle [(T.empty,"broadcaster",Low)]

pushUntilDone::App [Int]
pushUntilDone = do
        highSent' <- gets $ M.elems . thd3
        case sequenceA highSent' of
          Just stepsBySender -> return stepsBySender
          Nothing            -> pushButton >> pushUntilDone

signalCycle::[(Text,Text,Signal)] -> App ()
signalCycle signals = do
        signals' <- concat <$> traverse signalStep signals
        increaseCounter False . both length . partition ((==High) . thd3) $ signals'
        if null signals' then return ()
        else signalCycle signals'

signalStep::(Text,Text,Signal) -> App [(Text,Text,Signal)]
signalStep (fromId,mid,signal) = do
      mods <- gets fst3
      case mods !? mid of
        Nothing           -> return [] 
        Just m@Module{..} -> case (mdl, signal) of
            (Broadcaster,_)               -> send signal m
            (F _, High)                   -> return []
            (F f@FlipFlop{on=False}, Low) -> updateModule mid (F f{on=True})  >> send High m
            (F f@FlipFlop{on=True},  Low) -> updateModule mid (F f{on=False}) >> send Low m
            (C c@Conjunction{..}, _)      -> do
                      let inputPulses' = M.insert fromId signal inputPulses
                      updateModule mid (C c{inputPulses=inputPulses'})
                      if all (== High) $ M.elems inputPulses' 
                          then send Low m
                          else trackSenders mid >> send High m
  where send signal' = return . map (mid, ,signal') . destinations

increaseCounter :: Bool -> (Int,Int) -> App ()
increaseCounter increaseStepCounter (numHigh, numLow) = do
    Counter{..} <- gets snd3
    let stepCounter = if increaseStepCounter then succ steps else steps
    modify . second3 . const $ Counter{steps = stepCounter, cntHigh = cntHigh + numHigh, cntLow = cntLow + numLow}    

updateModule :: Text -> ModuleT -> App ()
updateModule mid m = modify . first3 $ M.update updateMod mid 
  where updateMod modul = Just modul{mdl=m}    

trackSenders :: Text -> App ()
trackSenders sender = do
    (_, Counter{..}, trackedSenders) <- get
    case trackedSenders !? sender of
      Just Nothing -> modify . third3 $ M.insert sender (Just steps)
      _            -> return ()
