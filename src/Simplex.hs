{-# LANGUAGE
   GADTs,
   MultiParamTypeClasses #-}

module Simplex(Simplex,
               geometricalDimension,
               topologicalDimension,
               subsimplex,
               subsimplices) where

import Spaces
import Data.List
import Combinatorics
import Utility
import Math.Combinatorics.Exact.Binomial


data Simplex where
-- | n-simplex represented by a list of vectors of given dimensionality
    Simplex :: (VectorSpace v) => Int -> -- ^ n, the topological dimension
                                  Int -> -- ^ the geometrical dimension
                                  [v] -> -- ^ the list of vectors
                                  Simplex

-- | The geometrical dimension of a simplex is the dimensionality of the
-- | underlying vector space.
geometricalDimension :: Simplex -> Int
geometricalDimension (Simplex nt ng _) = ng

-- | The topological dimension of an n-simplex is the number of vertices minus
-- | one.
topologicalDimension :: Simplex -> Int
topologicalDimension (Simplex nt ng _) = nt

-- | ith d-dimensional subsimplex of given subsimplex
subsimplex :: Simplex -> Int -> Int -> Simplex
subsimplex (Simplex k n l) d i = Simplex d n (map (l !!) indices)
    where indices = unrankIndices d i

-- | subsimplices of given dimension d
subsimplices :: Simplex -> Int -> [Simplex]
subsimplices s@(Simplex nt _ l) d = map (Simplex nt d) subsimplexLists
    where sublistIndices = map (unrankIndices d) [0..d `choose` (nt - 1)]
          subsimplexLists = map (takeIndices l) sublistIndices
