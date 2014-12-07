{-# LANGUAGE
   GADTs,
   MultiParamTypeClasses #-}

module Simplex(Simplex(Simplex),
               geometricalDimension,
               topologicalDimension,
               vertices,
               subsimplex,
               subsimplices) where

import Spaces
import Data.List
import Combinatorics
import Utility
import Math.Combinatorics.Exact.Binomial

-- | n-simplex represented by a list of vectors of given dimensionality
-- Invariant: geometrical dimension = length of the vector - 1
data Simplex v = Simplex [v] deriving ( Show )

-- | The geometrical dimension of a simplex is the dimensionality of the
-- | underlying vector space.
geometricalDimension :: VectorSpace v => Simplex v -> Int
geometricalDimension (Simplex []) =
    error "geometricalDimension: Encountered Simplex without vertices."
geometricalDimension (Simplex (l:ls)) = vspaceDim l


-- | The topological dimension of a n-simplex is the number of vertices minus
-- | one.
topologicalDimension :: Simplex v -> Int
topologicalDimension (Simplex []) =
    error "topologicalDimension: Encountered Simplex without vertices."
topologicalDimension (Simplex l) = (length l) - 1

-- | List of vertices of the simplex
vertices :: Simplex v -> [v]
vertices (Simplex l) = l

-- | i:th k-dimensional subsimplex of given simplex
subsimplex :: Simplex v -> Int -> Int -> Simplex v
subsimplex (Simplex []) _ _ =
                error "subsimplex: Encountered Simplex without vertices."
subsimplex s@(Simplex l) k i
           | k > n = error err_dim
           | i >= (n+1) `choose` (k+1) = error err_ind
           | otherwise = Simplex (map (l !!) indices)
    where n = topologicalDimension s
          indices = unrankIndices (k+1) i
          err_ind = "subsimplex: Index of subsimplex exceeds (n+1) choose (k+1)."
          err_dim = "subsimplex: Dimensionality of subsimplex is higher than that of the simplex."

-- | subsimplices of given dimension d
subsimplices :: Simplex v -> Int -> [Simplex v]
subsimplices s@(Simplex l) k
             | k > n = error err_dim
             | otherwise = map Simplex subsimplexLists
    where n = topologicalDimension s
          sublistIndices = map (unrankIndices (k+1)) [0..(n+1) `choose` (k+1) - 1]
          subsimplexLists = map (takeIndices l) sublistIndices
          err_dim = "subsimplices: Dimensionality of subsimplices is higher than that of the simplex."





