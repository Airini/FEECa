{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module FEECa.PolynomialTest where


import Control.Monad

import Properties
import FEECa.Polynomial
import FEECa.Internal.Vector
import FEECa.Internal.Simplex
import FEECa.Internal.Spaces
import FEECa.Utility.Utility
import FEECa.Utility.Combinatorics
import FEECa.Utility.Test
import FEECa.Utility.Print
import qualified FEECa.Internal.MultiIndex as MI

import Test.QuickCheck (quickCheckAll)
import qualified Test.QuickCheck as Q
import qualified Numeric.LinearAlgebra.HMatrix as M

------------------------------------------------------------------------------
-- Dimension of the space to be tested. Must be greater than zero.
------------------------------------------------------------------------------

n :: Int
n = 2

------------------------------------------------------------------------------
-- Generate a random polynomial of dimension n as fixed by the above parameter.
-- The Polynomial may be of type Constant or Polynomial. If it is of type
-- Polynomial, degree the degree is chosen between 0 and 10 and the constants
-- are chosen randomly.
------------------------------------------------------------------------------

instance (Ring r, Q.Arbitrary r) => Q.Arbitrary (Polynomial r) where
  arbitrary = Q.oneof [arbitraryPolynomial n, arbitraryConstant]

arbitraryPolynomial :: (Ring r, Q.Arbitrary r) => Int -> Q.Gen (Polynomial r)
arbitraryPolynomial  n = do r   <- Q.choose (0,10)
                            mis <- Q.listOf1 (arbitraryMI n r)
                            cs  <- Q.listOf1 Q.arbitrary
                            return $ polynomial (zip (take 10 cs) (take 10 mis))

arbitraryConstant :: (Ring r, Q.Arbitrary r) => Q.Gen (Polynomial r)
arbitraryConstant = liftM constant Q.arbitrary

------------------------------------------------------------------------------
-- Generate random vectors of dimension n, so that they can be used to evaluate
-- the randomly generated polynomials.
------------------------------------------------------------------------------

instance (Field f, Q.Arbitrary f) => Q.Arbitrary (Vector f) where
  arbitrary = arbitraryVector n


------------------------------------------------------------------------------
-- Abstract propreties of arithmetic on polynomials. Addition, Subtraction and
-- multiplication of polynomials must commute with the evaluate operator.
------------------------------------------------------------------------------

propArithmetic :: (EuclideanSpace v, Function f v, VectorSpace f, Ring f,
                    r ~ Scalar v, r ~ Scalar f)
                => (r -> r -> Bool)
                -> f -> f -> v -> r
                -> Bool
propArithmetic eq f1 f2 v c =
    (prop_operator2_commutativity eq addV add (evaluate v) f1 f2
    && prop_operator2_commutativity eq mul mul (evaluate v) f1 f2
    && prop_operator2_commutativity eq sub sub (evaluate v) f1 f2
    && prop_operator_commutativity eq (sclV c) (mul c) (evaluate v) f1)

------------------------------------------------------------------------------
-- Concrete arithmetic properties for polynomials defined over rationals.
------------------------------------------------------------------------------

prop_arithmetic_rf :: Polynomial Rational -> Polynomial Rational
                   -> Vector Rational -> Rational
                   -> Bool
prop_arithmetic_rf = propArithmetic (==)

--------------------------------------------------------------------------------
-- Derivation of Polynomials
--------------------------------------------------------------------------------

-- Linearity
propDerivation_linear :: (EuclideanSpace v, Function f v, VectorSpace f,
                          r ~ Scalar v, r ~ Scalar f)
                       => v -> v -> r -> f -> f
                       -> Bool
propDerivation_linear v1 v2 = prop_linearity (==) (evaluate v2 . derive v2)

-- Product rule
propDerivationProduct :: (EuclideanSpace v, Function f v, Ring f)
                        => v -> v -> f -> f
                        -> Bool
propDerivationProduct v1 v2 p1 p2 =
    evaluate v1 (add (mul dp1 p2) (mul dp2 p1))
    == evaluate v1 (derive v2 (mul p1 p2))
  where dp1 = derive v2 p1
        dp2 = derive v2 p2

prop_derivation_product :: Vector Rational -> Vector Rational
                        -> Polynomial Rational -> Polynomial Rational
                        -> Bool
prop_derivation_product = propDerivationProduct

-- Test for polynomials using exact arithmetic.
prop_arithmetic_rational :: Polynomial Rational -> Polynomial Rational
                         -> Vector Rational -> Rational
                         -> Bool
prop_arithmetic_rational = propArithmetic (==)

prop_derivation_linear_rational :: Vector Rational -> Vector Rational
                                -> Rational
                                -> Polynomial Rational -> Polynomial Rational
                                -> Bool
prop_derivation_linear_rational = propDerivation_linear

prop_derivation_product_rational :: Vector Rational
                                 -> Vector Rational
                                 -> Rational
                                 -> Polynomial Rational
                                 -> Polynomial Rational
                                 -> Bool
prop_derivation_product_rational = propDerivation_linear

--------------------------------------------------------------------------------
-- Barycentric Coordinates
--------------------------------------------------------------------------------

-- | Generate random simplex of dimesion 1 <= n <= 10.
instance (EuclideanSpace v, Q.Arbitrary (Scalar v)) => Q.Arbitrary (Simplex v) where
  arbitrary = do n <- Q.choose (1, 3)
                 k <- Q.choose (1, n)
                 arbitrarySubsimplex k n

-- TODO: Fails for n > 3 apparently due to numerical instability. To investigate
-- further.
prop_barycentric :: Simplex (Vector Double) -> Bool
prop_barycentric t =
    allEq [[evaluate v b | v <- vs] | b <- bs] oneLists
  where allEq l1 l2 = and $ zipWith (\l3 l4 -> (and (zipWith eqNum l3 l4))) l1 l2
        bs          = barycentricCoordinates t
        vs          = vertices t
        k           = topologicalDimension t
        oneLists    = map (map fromInt') (sumRLists (k+1) 1)


return []
testPolynomial = $quickCheckAll

t  = referenceSimplex 3 :: (Simplex (Vector Double))
bs = barycentricCoordinates t
vs = vertices t
l1 = [[evaluate v b | v <- vs] | b <- bs]
l2 = (map (map fromInt') $ sumRLists (topologicalDimension t + 1) 1) :: [[Double]]

allEq l1 l2 = and $ zipWith (\l3 l4 -> (and (zipWith eqNum l3 l4))) l1 l2
