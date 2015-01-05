{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Polynomials where

import Spaces
import Simplex
import Data.Maybe
import Data.List
import qualified Numeric.LinearAlgebra.HMatrix as M
import qualified Numeric.LinearAlgebra.Data as M

data Polynomial a = Polynomial  [(a,[Int])] deriving Show

-- | Create 1st degree homogeneous polynomial in n variables from
-- | length n list of coefficients.
deg1P :: Field a => [a] -> Polynomial a
deg1P ns = Polynomial $ zip ns linP
        where dim = (length ns)
              linP = [[if (i==j) then 1 else 0 | j <- [1..dim]] | i <- [1..dim]]

-- | Create 0th degree polynomial from given scalar
deg0P :: Int -> a -> Polynomial a
deg0P n c = Polynomial [(c,replicate n 0)]

-- | Add two polynomials
addP :: Polynomial a -> Polynomial a -> Polynomial a
addP (Polynomial p1) (Polynomial p2) = Polynomial (p1 ++ p2)


-- | Evaluate polynomial at given point in space
evalP :: (VectorSpace v, Floating (Fieldf v))
         => (Polynomial (Fieldf v)) -> v -> Fieldf v
evalP (Polynomial []) v = addId
evalP (Polynomial ((c,alpha):ls)) v = add (mul c (powV v alpha))
                                          (evalP (Polynomial ls) v)

-- | 1st degree polynomial taking value 1 on vertex n_i of the simplex and
-- | 0 on all others. Requires the topological dimension of the simplex to be
-- | as large as the geometrical dimension, i.e. the simplex must contain n+1
-- | vertices if the underlying space has dimensionality n.
barycentricCoord :: (VectorSpace v, (Fieldf v) ~ Double) => Simplex v -> Int -> Polynomial (Fieldf v)
barycentricCoord s i = addP (deg1P (drop 1 c)) (deg0P dim (c!!0))
    where ns = vertices s
          dim = topologicalDimension s
          dim1 = dim + 1
          a = foldr (\ x y -> (1:x) ++ y) [] (map toList ns)
          b = ((replicate i addId) ++ [mulId]) ++ (replicate (dim - i) 0)
          c = concat (M.toLists $ fromJust $ (M.linearSolve ((dim1 M.>< dim1) a)
              ((dim1 M.>< 1) b)))


