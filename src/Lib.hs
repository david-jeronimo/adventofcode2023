module Lib
    ( Part(..), Text, CyclicEnum(..)) where

import Data.Text (Text)

data Part = PartOne | PartTwo deriving Show

class (Eq a, Bounded a, Enum a) => CyclicEnum a where
  predC::a -> a
  predC a | a == minBound = maxBound
          | otherwise     = pred a
  succC::a -> a
  succC a | a == maxBound = minBound
          | otherwise     = succ a
