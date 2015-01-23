{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances#-}

module Polynomials where

import Spaces
import Simplex
import Data.Maybe
import Data.List
import qualified Numeric.LinearAlgebra.HMatrix as M
import qualified Numeric.LinearAlgebra.Data as M


-- TODO: relocate, here for now
instance Floating a => Field a where
    add = (+)
    addId = 0
    addInv = (0-)
    mul = (*)
    mulId = 1
    mulInv = (1/)
    fromInt = fromInteger . toInteger

-- | Type synonym for multi-indices to specify monomials over R^n. The i-th integer
-- | in the list specified the power of the corresponding component of R^n. The degree
-- | of the monomial is given by the sum of the non-negative entries.
type MultiIndex = [Int]

-- | Polynomials as list of coefficient-monomial terms over R^n.
data Polynomial a = Polynomial  [(a,MultiIndex)] deriving Show

instance (Rn v, a ~ (Fieldf v), Floating a, Eq a) => Function (Polynomial a) v a where
    deriv = deriveP
    eval  = evalP

-- | Directional derivative of a polynomial in a given space direction.
deriveP :: (Rn v, a ~ (Fieldf v), Floating a, Eq a) => v -> Polynomial a -> Polynomial a
deriveP v (Polynomial ps) = Polynomial $ concat [deriveMonomial (toList v) m
                                                     | m <- ps]
deriveMonomial :: (Floating a, Eq a) => [a] -> (a,MultiIndex) -> [(a,MultiIndex)]
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
evalP :: (Rn v, a ~ (Fieldf v), Floating a)
         =>  v -> Polynomial a -> a
evalP v (Polynomial []) = addId
evalP v (Polynomial ((c,alpha):ls)) = add (mul c (powV v alpha))
                                          (evalP v (Polynomial ls))

-- | 1st degree polynomial taking value 1 on vertex n_i of the simplex and
-- | 0 on all others. Requires the topological dimension of the simplex to be
-- | as large as the geometrical dimension, i.e. the simplex must contain n+1
-- | vertices if the underlying space has dimensionality n.
barycentricCoord :: (Rn v, (Fieldf v) ~ Double) => Simplex v -> Int -> Polynomial (Fieldf v)
barycentricCoord s i = addP (deg1P (drop 1 c)) (deg0P dim (c!!0))
    where ns = vertices s
          dim = topologicalDimension s
          dim1 = dim + 1
          a = foldr (\ x y -> (1:x) ++ y) [] (map toList ns)
          b = ((replicate i addId) ++ [mulId]) ++ (replicate (dim - i) 0)
          c = concat (M.toLists $ fromJust $ (M.linearSolve ((dim1 M.>< dim1) a)
              ((dim1 M.>< 1) b)))


