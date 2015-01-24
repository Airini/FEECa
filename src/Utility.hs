module Utility(takeIndices) where

takeIndices :: [a] -> [Int] -> [a]
takeIndices l indices = map (l !!) indices
