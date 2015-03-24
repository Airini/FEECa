{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances#-}

module Polynomials(Polynomial(..),
                   deg1P,
                   deg0P,
                   zeroP,
                   addP,
                   barycentricCoordinates,
                   barycentricCoordinate) where

import Spaces hiding (toList)
import Simplex
import Vector
import Point
import Print (printPolynomial)
import Data.Maybe (fromJust)
import Data.List

import MultiIndex(MultiIndex, zeroMI, oneMI, decMI, toListMI)
import qualified Numeric.LinearAlgebra.HMatrix as M
import qualified Numeric.LinearAlgebra.Data as M

-- | Polynomials as list of coefficient-monomial terms over R^n.
data Polynomial a = Polynomial  [(a,MultiIndex)]

instance Function (Polynomial Double) Vector where
  type Values (Polynomial Double) Vector = Double
  deriv = deriveP
  eval  = evalP

instance Show (Polynomial Double) where
    show (Polynomial p) = show $ printPolynomial "x" p

polynomial :: [(Double, MultiIndex)] -> Polynomial Double
polynomial l = Polynomial $ removeZeros l
    where removeZeros l = [(c,a) | (c,a) <- l, c /= 0.0]

-- | Directional derivative of a polynomial in a given space direction.
deriveP :: Vector -> Polynomial Double -> Polynomial Double
deriveP v (Polynomial ps) = Polynomial $ concatMap (deriveMonomial (toList v)) ps

deriveMonomial :: [Double] -> (Double,MultiIndex) -> [(Double,MultiIndex)]
deriveMonomial vs (c,a)
  | length vs == dim a = [(c', decMI i a)
                                | i <- [0..(dim a)-1],
                                  let c' = mul (vs!!i) (mul c (fromInt (a'!!i))),
                                  c' /= 0]
  | otherwise = error "deriveMonomial: Direction and multi-index have unequal lengths"
  where a' = toListMI a

gradP :: Polynomial Double -> [Polynomial Double]
gradP (Polynomial ps) = map polynomial (transpose grads)
    where grads = map gradMonomial ps

gradMonomial :: (Double,MultiIndex) -> [(Double,MultiIndex)]
gradMonomial (c,a) = [(c', decMI i a)
                          | i <- [0..(dim a)-1],
                            let c' = mul c (fromInt (a'!!i))]
    where a' = toListMI a

-- | Create 1st degree homogeneous polynomial in n variables from
-- | length n list of coefficients. The coefficient with index i in the list
-- | equals the coefficient of the ith variable of the returned polynomial.
deg1P :: Field a => [a] -> Polynomial a
deg1P ns = Polynomial $ zip ns [oneMI dim i | i <- [0..dim-1]]
  where dim  = length ns


-- | Create 0th degree polynomial from given scalar
deg0P :: Int -> a -> Polynomial a
deg0P n c = Polynomial [(c, zeroMI n)]

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
barycentricCoordinates :: Simplex -> [Polynomial Double]
barycentricCoordinates s = map vectorToPoly (take (nt+1) (M.toColumns mat))
    where mat = M.inv (simplexToMatrix (extendSimplex s))
          n = geometricalDimension s
          nt = topologicalDimension s

-- | Simple wrapper for barycentricCoordinates that picks out the ith polynomial
-- | in the list
barycentricCoordinate :: Simplex -> Int -> Polynomial Double
barycentricCoordinate s i = barycentricCoordinates s !! i

simplexToMatrix :: Simplex -> M.Matrix Double
simplexToMatrix s@(Simplex l) = M.matrix (n+1) (concatMap append1 l)
    where n = geometricalDimension s
          append1 p = 1 : toList (fromPoint p)

vectorToPoly :: M.Vector Double -> Polynomial Double
vectorToPoly v = addP (deg0P n (head l)) (deg1P (tail l))
    where l = M.toList v
          n = length l - 1
