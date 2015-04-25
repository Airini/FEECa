module PolynomialSpaces where

import Bernstein
import Combinatorics
import Forms
import MultiIndex
import Polynomials
import Simplex
import Spaces

-- | Lists all basis functions of a basis of the space of degree-r
-- | k-differential forms over the given vertex t.
prBasis :: Simplex -> Int -> Int -> [Form BernsteinPolynomial]
prBasis t r k = [ Fform k n [ (p, sigma) ] | p <- degRPolynomials t n r,
                                             sigma <- increasingLists n k]
    where n = geometricalDimension t

-- | Lists all basis functions of the space Pr minus space of k-differential
-- | forms over the given vertex.
prMinusBasis :: Simplex -> Int -> Int -> [Form BernsteinPolynomial]
prMinusBasis t r k = [ sclV (lambda mi) (wF sigma) | mi <- degRMI n k,
                                                    sigma <- increasingLists' n k,
                                                    cond mi  sigma]
                     where cond mi sigma = isZeroMI (minimum sigma) mi
                           lambda = bernsteinMonomial t
                           wF = whitneyForm t
                           n = geometricalDimension t

whitneyForm :: Simplex -> [Int] -> Form BernsteinPolynomial
whitneyForm t ls = Fform k n [( lambda' i, subsets !! i) | i <- [0..k]]
    where k = (length ls) - 1
          n = geometricalDimension t
          subsets = sublists1 ls
          lambda' i = sclV ((-1)^i) (Bernstein t (monomial (oneMI n i)))

