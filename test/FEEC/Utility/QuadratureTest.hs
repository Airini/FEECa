module QuadratureTest where

import Quadrature
import Test.QuickCheck
import Utility

-- | Evaluate Jacobi polynomials at the Gauss quadrature nodes and check
-- | if they are approximately zeros as they should be. Does not check for
-- | high a, b, n, but may be due to numerical instability.
-- FIXME: fails!! input case: 1 2 0
prop_jacobi_zeros :: Int -> Int -> Property
prop_jacobi_zeros n alpha = ((n > 0) && (n < 10) && (alpha >= 1) && (alpha <= 10)) ==>
                 all (eqNum 0.0) [jacobiP alpha alpha n x | x <- gjnodes]
    where gjnodes = map fst (gaussJacobiQuadrature alpha alpha n)

-- | Gauss quadrature nodes weights must sum to 2
prop_sum_2 :: Int -> Property
prop_sum_2 n = (n > 0) ==> eqNum 2.0 (sum weights)
    where weights = map snd (gaussJacobiQuadrature 0 0 n)

-- | Evaluate Legendre polynomials at the Gauss quadrature nodes and check
-- | if they are approximately zeros as they should be.
prop_legendre_zeros :: Int -> Property
prop_legendre_zeros n = ((n > 0) && (n < 100)) ==>
                 all (eqNum 0.0) [legendreP n x | x <- gnodes]
    where gnodes = map fst (gaussLegendreQuadrature n)
