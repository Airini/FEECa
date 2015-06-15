{-# LANGUAGE
   GADTs,
   MultiParamTypeClasses #-}

module Simplex( extendSimplex,
                directionVectors,
                geometricalDimension,
                referencePoint,
                referenceSimplex,
                simplex,
                simplex',
                Simplex(..),
                subsimplex,
                subsimplices,
                topologicalDimension,
                vertices,
                volume ) where

import Spaces
import Point
import Vector
import Data.List
import Combinatorics
import Print
import Utility
import GramSchmidt
import Math.Combinatorics.Exact.Binomial
import Math.Combinatorics.Exact.Factorial
import qualified Numeric.LinearAlgebra.HMatrix as M

-- | n-simplex represented by a list of vectors of given dimensionality
-- | Invariant: geometrical dimension = length of the vector - 1
data Simplex = Simplex [Point] deriving (Eq)

instance Show Simplex where
    show (Simplex l) = "Simplex:\n" ++ show (printVectorColl 2 l) ++ "\n"

-- | Create simplex from a given list of points in R^n
simplex :: [Point] -> Simplex
simplex = Simplex

-- | Create a n+1 simplex from a refence point and n direction vectors
simplex' :: Point -> [Vector] -> Simplex
simplex' p0 vs = Simplex (p0:l)
    where l = map (toPoint . addV (fromPoint p0)) vs

-- | The geometrical dimension of a simplex is the dimensionality of the
-- | underlying vector space.
geometricalDimension :: Simplex -> Int
geometricalDimension (Simplex []) =
    error "geometricalDimension: Encountered Simplex without vertices."
geometricalDimension (Simplex (p:ps)) = dimP p

-- | The topological dimension of a n-simplex is the number of vertices minus
-- | one.
topologicalDimension :: Simplex -> Int
topologicalDimension (Simplex []) =
    error "topologicalDimension: Encountered Simplex without vertices."
topologicalDimension (Simplex l) = length l - 1

-- | List of vertices of the simplex
vertices :: Simplex -> [Point]
vertices (Simplex l) = l

-- | Reference point of the simplex, i.e. the first point in the list
-- | of vectors
referencePoint :: Simplex -> Point
referencePoint (Simplex (p:ps)) = p
referencePoint (Simplex []) = error "reference Point: Simplex contains no points"

-- | List of the n direction vector pointing from the first point of the
-- | simplex to the others.
directionVectors :: Simplex -> [Vector]
directionVectors (Simplex (p:ps)) = map (\x -> subV (fromPoint x) v) ps
    where v = fromPoint p
directionVectors (Simplex _) = []

-- | i:th k-dimensional subsimplex of given simplex
subsimplex :: Simplex -> Int -> Int -> Simplex
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
subsimplices :: Simplex -> Int -> [Simplex]
subsimplices s@(Simplex l) k
             | k > n = error err_dim
             | otherwise = map Simplex subsimplexLists
    where n = topologicalDimension s
          sublistIndices = map (unrankIndices (k+1)) [0..(n+1) `choose` (k+1) - 1]
          subsimplexLists = map (takeIndices l) sublistIndices
          err_dim = "subsimplices: Dimensionality of subsimplices is higher than that of the simplex."

-- | Reference simplex in R^n
referenceSimplex :: Int -> Simplex
referenceSimplex n = Simplex $ origin n : [unitP n i | i <- [0..n-1]]

extendSimplex :: Simplex -> Simplex
extendSimplex s@(Simplex ps)
              | n == nt = s
              | otherwise = simplex' p0 (reverse (gramSchmidt' dirs unitvs))
    where n = geometricalDimension s
          nt = topologicalDimension s
          dirs = directionVectors s
          p0 = referencePoint s
          unitvs = [unitV n i | i <- [0..n-1]]

-- | Computes the k-dimensional volume (Lebesgue measure) of a simplex
-- | in n dimensions using the Gram Determinant rule.
volume :: Simplex -> Double
volume t = sqrt (abs (M.det (M.mul w wT))) / fromInteger (factorial k)
    where k = topologicalDimension t
          n = geometricalDimension t
          w = M.matrix n (concatMap toList (directionVectors t))
          wT = M.tr w
