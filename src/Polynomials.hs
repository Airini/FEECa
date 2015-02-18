{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances#-}

module Polynomials where

import Spaces hiding (toList)
import Simplex
import Vector
import Point
import Data.Maybe (fromJust)
import qualified Numeric.LinearAlgebra.HMatrix as M
import qualified Numeric.LinearAlgebra.Data as M


-- | Type synonym for multi-indices to specify monomials over R^n. The i-th integer
-- | in the list specified the power of the corresponding component of R^n. The degree
-- | of the monomial is given by the sum of the non-negative entries.
type MultiIndex = [Integer]

-- | Polynomials as list of coefficient-monomial terms over R^n.
data Polynomial a = Polynomial  [(a,MultiIndex)] deriving Show

instance Function (Polynomial Double) Vector where
  type Values (Polynomial Double) Vector = Double
  deriv = deriveP
  eval  = evalP

-- | Directional derivative of a polynomial in a given space direction.
deriveP :: Vector -> Polynomial Double -> Polynomial Double
deriveP v (Polynomial ps) = Polynomial $ concatMap (deriveMonomial (toList v)) ps

deriveMonomial :: [Double] -> (Double,MultiIndex) -> [(Double,MultiIndex)]
deriveMonomial vs (c,a)
  | length vs == length a = [(c', decInd i a)
                                | i <- [0..length a-1],
                                  let c' = mul (vs!!i) (mul c (fromInt (a!!i))),
                                  c' /= 0]
  | otherwise = error "deriveMonomial: Direction and multi-index have unequal lengths"

-- | Decrease element in multi-index
decInd :: Int -> MultiIndex -> MultiIndex
decInd i a
  | (i >= 0) && (i < length a) = take i a ++ ((max 0 (a!!i)-1) : drop (i+1) a)
  | otherwise = error "decInd: Illegal index"


-- | Create 1st degree homogeneous polynomial in n variables from
-- | length n list of coefficients. The coefficient with index i in the list
-- | equals the coefficient of the ith variable of the returned polynomial.
deg1P :: Field a => [a] -> Polynomial a
deg1P ns = Polynomial $ zip ns linP
  where dim  = length ns
        linP = [[if i==j then 1 else 0 | j <- [1..dim]] | i <- [1..dim]]

-- | Create 0th degree polynomial from given scalar
deg0P :: Int -> a -> Polynomial a
deg0P n c = Polynomial [(c,replicate n 0)]

-- | The zero polynomial
zeroP :: Polynomial a
zeroP = Polynomial []

-- | Add two polynomials
addP :: Polynomial a -> Polynomial a -> Polynomial a
addP (Polynomial p1) (Polynomial p2) = Polynomial (p1 ++ p2)

-- | Evaluate polynomial at given point in space
evalP :: Vector -> Polynomial Double -> Double
evalP v (Polynomial []) = addId
evalP v (Polynomial ((c,alpha):ls)) = add (mul c (powV v alpha))
                                          (evalP v (Polynomial ls))

-- | 1st degree polynomial taking value 1 on vertex n_i of the simplex and
-- | 0 on all others. Requires the topological dimension of the simplex to be
-- | as large as the geometrical dimension, i.e. the simplex must contain n+1
-- | vertices if the underlying space has dimensionality n.
barycentricCoords :: Simplex -> [Polynomial Double]
barycentricCoords s = map vectorToPoly (take (nt+1) (M.toColumns mat))
    where mat = M.inv (simplexToMatrix (extendSimplex s))
          n = geometricalDimension s
          nt = topologicalDimension s

-- | Simple wrapper for barycentricCoords that picks out the ith polynomial
-- | in the list
barycentricCoord :: Simplex -> Int -> Polynomial Double
barycentricCoord s i = barycentricCoords s !! i

simplexToMatrix :: Simplex -> M.Matrix Double
simplexToMatrix s@(Simplex l) = M.matrix (n+1) (concatMap append1 l)
    where n = geometricalDimension s
          append1 p = 1 : toList (fromPoint p)

vectorToPoly :: M.Vector Double -> Polynomial Double
vectorToPoly v = addP (deg0P n (head l)) (deg1P (tail l))
    where l = M.toList v
          n = length l - 1
