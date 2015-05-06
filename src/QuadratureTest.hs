module QuadratureTest where

import Quadrature
import Test.QuickCheck

-- | Evaluate Jacobi polynomials at the Gauss quadrature nodes and check
-- | if they are approximately zeros as they should be. Does not check for
-- | high a, b, n, but may be due to numerical instability.
-- FIXME: fails!! input case: 1 2 0
prop_jacobi_zeros :: Int -> Int -> Int -> Property
prop_jacobi_zeros n a b  = ((n > 0) && (n < 10) && (a >= 1) && (a <= 10)) ==>
                 all (eqNum 0.0) [jacobiP n a a x | x <- gjnodes]
    where gjnodes = map fst (gaussJacobiQuadrature n a b)

-- | Evaluate Legendre polynomials at the Gauss quadrature nodes and check
-- | if they are approximately zeros as they should be.
prop_legendre_zeros :: Int -> Property
prop_legendre_zeros n = ((n > 0) && (n < 100)) ==>
                 all (eqNum 0.0) [legendreP n x | x <- gnodes]
    where gnodes = map fst (gaussLegendreQuadrature n)

-- | Numerical equality accounting for round-off errors
eqNum :: Double -> Double -> Bool
eqNum a b = abs (a - b) < 2e-11
