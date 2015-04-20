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
                   barycentricCoordinate,
                   constant) where

import Spaces hiding (toList)
import Simplex
import Vector
import Point
import Utility(pairM)
import Print (printPolynomial, printConstant)
import Data.Maybe (fromJust)
import Data.List

import MultiIndex(MultiIndex, zeroMI, oneMI, decMI, toListMI, addMI)
import qualified Numeric.LinearAlgebra.HMatrix as M
import qualified Numeric.LinearAlgebra.Data as M

-- | Polynomials as list of coefficient-monomial terms over R^n.
data Polynomial a = Polynomial Int [(a,MultiIndex)]
                  | Constant a

instance Function (Polynomial Double) Vector where
  type Values (Polynomial Double) Vector = Double
  deriv = deriveP
  eval  = evalP

instance Show (Polynomial Double) where
    show (Polynomial _ p) = show $ printPolynomial "x" p
    show (Constant c) = show $ printConstant c

-- | Polynomial type as a functor
instance Functor Polynomial where
  fmap f (Polynomial n ms) = Polynomial n (map (pairM f id) ms)
  fmap f (Constant c) = Constant $ f c

-- | Polynomial as a vector space
instance (Field f) => VectorSpace (Polynomial f) where
  type Fieldf (Polynomial f) = f
  vspaceDim _ = undefined
  addV = addP
  sclV = sclP

-- | 'PolyN' as a field
instance (Field f) => Field (Polynomial f) where
  add = addP
  addId  = Constant addId
  addInv  = sclP (addInv addId)

  mul = mulP
  mulId     = Constant mulId
  mulInv    = undefined
  fromInt x = Constant (fromInt x)
  -- add more efficient exponentiation?

polynomial :: Int -> [(Double, MultiIndex)] -> Polynomial Double
polynomial n l = Polynomial n $ removeZeros l
    where removeZeros l = [(c,a) | (c,a) <- l, c /= 0.0]

constant :: Double -> Polynomial Double
constant c = Constant c

-- | Directional derivative of a polynomial in a given space direction.
deriveP :: Vector -> Polynomial Double -> Polynomial Double
deriveP v (Polynomial n ps) = Polynomial n $ concatMap (deriveMonomial (toList v)) ps
deriveP _ (Constant _) = Constant 0

deriveMonomial :: [Double] -> (Double,MultiIndex) -> [(Double,MultiIndex)]
deriveMonomial vs (c,a)
  | length vs == dim a = [(c', decMI i a)
                                | i <- [0..(dim a)-1],
                                  let c' = mul (vs!!i) (mul c (fromInt (a'!!i))),
                                  c' /= 0]
  | otherwise = error "deriveMonomial: Direction and multi-index have unequal lengths"
  where a' = toListMI a

-- gradP :: Polynomial Double -> [Polynomial Double]
-- gradP (Polynomial ps) = map polynomial (transpose grads)
--     where grads = map gradMonomial ps

-- gradMonomial :: (Double,MultiIndex) -> [(Double,MultiIndex)]
-- gradMonomial (c,a) = [(c', decMI i a)
--                           | i <- [0..(dim a)-1],
--                             let c' = mul c (fromInt (a'!!i))]
--     where a' = toListMI a

-- | Create 1st degree homogeneous polynomial in n variables from
-- | length n list of coefficients. The coefficient with index i in the list
-- | equals the coefficient of the ith variable of the returned polynomial.
deg1P :: Field a => [a] -> Polynomial a
deg1P ns = Polynomial dim $ zip ns [oneMI dim i | i <- [0..dim-1]]
  where dim  = length ns

-- | Create 0th degree polynomial from given scalar
deg0P :: Int -> a -> Polynomial a
deg0P n c = Polynomial n [(c, zeroMI n)]

-- | The zero polynomial
zeroP :: Num a => Polynomial a
zeroP = Constant (fromInteger 0)

-- | Add two polynomials
addP :: Field a => Polynomial a -> Polynomial a -> Polynomial a
addP (Polynomial n1 p1) (Polynomial n2 p2)
     | n1 == n2 = Polynomial n1 (p1 ++ p2)
     | otherwise = error "addP: Polynomials have different dimensionalities."
addP (Polynomial n1 p1) (Constant c) = Polynomial n1 $ (c,(zeroMI n1)):p1
addP (Constant c1) (Constant c2) = Constant $ add c1 c2
addP p1 p2 = addP p2 p1

-- | Polynomial multiplication
mulP :: Field a => Polynomial a -> Polynomial a -> Polynomial a
mulP (Polynomial n1 ms) (Polynomial n2 ns) = Polynomial n1 [termMul x y | x <- ms, y <- ns]
  where termMul (a,as) (b,bs) = (mul a b, addMI as bs)
mulP (Constant a) p2 = sclP a p2
mulP p1 p2 = mulP p2 p1

-- | Scaling of a polynomial
sclP :: Field a => a -> Polynomial a -> Polynomial a
sclP x = fmap (mul x)

-- | Evaluate polynomial at given point in space
evalP :: Vector -> Polynomial Double -> Double
evalP v (Polynomial n []) = addId
evalP v (Polynomial n ((c,alpha):ls)) = add (mul c (powV v alpha))
                                        (evalP v (Polynomial n ls))
evalP _ (Constant c) = c

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
