{-# LANGUAGE TemplateHaskell #-}

module FEECa.Utility.QuadratureTest (
    testQuadrature
) where

import FEECa.Utility.Quadrature
import FEECa.Utility.Utility
import FEECa.Utility.Test

import Test.QuickCheck

-- | Evaluate Jacobi polynomials at the Gauss quadrature nodes and check
-- | if they are approximately zeros as they should be. Does not check for
-- | high a, b, n, but may be due to numerical instability.
-- FIXME: fails!! input case: 1 2 0
prop_jacobi_zeros :: SmallInt -> SmallInt -> Bool
prop_jacobi_zeros (SmallInt n) (SmallInt alpha) =
    all (eqNum 0.0) [ jacobiP alpha alpha n x | (x,_) <- gjnodes ]
  where gjnodes = gaussJacobiQuadrature alpha alpha n

-- | Gauss quadrature nodes weights must sum to 1
prop_sum_1 :: Int -> Property
prop_sum_1 n = n > 0 ==> eqNum 1.0 (sum weights)
  where weights = map snd (gaussJacobiQuadrature 0 0 n)


return []
testQuadrature = $quickCheckAll

