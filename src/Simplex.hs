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
    -- Invariant: geometrical dimension = length of the vector - 1

geometricalDimension :: Simplex -> Int
geometricalDimension (Simplex nt ng _) = ng

topologicalDimension :: Simplex -> Int
topologicalDimension (Simplex nt ng _) = nt

-- subsimplex s k i has picks the i:th subsimplex of s of dimension k 
--   (out of the choose ng k possible)
subsimplex :: Simplex -> Int -> Int -> Simplex
subsimplex (Simplex nt ng l) k i = Simplex nt k $ map (l !!) indices
    where indices = unrank k i

-- Perhaps add the parameter to the type. 
-- map :: (v -> w) -> Sim v -> Sim w

-- f :: VectorSpace v => Simplex v -> something


data H where H :: Num a => a -> Int -> H

-- functions from H??

f :: Num b => H -> b
f (H x n) = x + fromInteger 3



