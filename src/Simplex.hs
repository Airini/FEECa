{-# LANGUAGE
   GADTs,
   MultiParamTypeClasses #-}

module Simplex where

import Spaces
import Data.List
import Combinatorics

data Simplex where
-- | n-simplex represented by a list of vectors of given dimensionality
    Simplex :: (VectorSpace v) => Int -> -- ^ n, the topological dimension
                                  Int -> -- ^ the geometrical dimension
                                  [v] -> -- ^ the list of vectors
                                  Simplex

geometricalDimension :: Simplex -> Int
geometricalDimension (Simplex nt ng _) = ng

topologicalDimension :: Simplex -> Int
topologicalDimension (Simplex nt ng _) = nt

subsimplex :: Simplex -> Int -> Int -> Simplex
subsimplex (Simplex l) k i = map (l !!) indices
    where indices = unrank k i
