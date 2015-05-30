{-# LANGUAGE FlexibleInstances #-}

module FiniteElementSpace where

import Bernstein
import Combinatorics
import Form
import qualified MultiIndex as MI
import Polynomial
import Print
import Simplex
import Spaces

instance Show (Form BernsteinPolynomial) where
  show (Form k n cs) = show (printForm dlambda "0" show cs)

-- | List all basis function of the space of polynomials of degree r
-- | over the simplex.
prBasis :: Simplex -> Int -> [BernsteinPolynomial]
prBasis t r = [ Bernstein t (monomial mi) | mi <- MI.degR n r ]
    where n = geometricalDimension t

-- | List all basis functions of the space of k-differential
-- | forms of degree 1 over the given vertex.
p1MinusBasis :: Simplex -> Int -> [Form BernsteinPolynomial]
p1MinusBasis t k = [ whitneyForm t ls | ls <- increasingLists' n k ]
    where n = geometricalDimension t

whitneyForm :: Simplex -> [Int] -> Form BernsteinPolynomial
whitneyForm t ls = Form k n [( lambda' i, subsets !! i) | i <- [0..k]]
    where k = length ls - 1
          n = geometricalDimension t
          subsets = sublists1 ls
          lambda' i = sclV ((-1)^i) (Bernstein t (monomial (MI.one n i)))

