{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module FEECa.PolynomialTest where


import Control.Monad    ( liftM, liftM2 )

import FEECa.Internal.Vector
import FEECa.Internal.Simplex
import FEECa.Internal.Spaces
import FEECa.Polynomial

import FEECa.Utility.Utility
import FEECa.Utility.Combinatorics
import FEECa.Utility.Print

import Properties
import FEECa.Utility.Test
import qualified Test.QuickCheck as Q


-- =======
-- import qualified FEECa.Internal.MultiIndex as MI

--import qualified Numeric.LinearAlgebra.HMatrix as M
-- >>>>>>> efa221f... Cleaned up further tests (definitely shouldn't use `fromDouble`).

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
  arbitrary = Q.frequency [(4,arbitraryPolynomial n), (1,arbitraryConstant)]

arbitraryPolynomial :: (Ring r, Q.Arbitrary r) => Int -> Q.Gen (Polynomial r)
arbitraryPolynomial n = do
  r <- Q.choose (0,10)
  s <- Q.choose (0,15)
  let mis = Q.vectorOf s (arbitraryMI n r)
      cs  = Q.vectorOf s Q.arbitrary
  liftM polynomial (liftM2 zip cs mis)

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
propArithmetic eq x y v c =
     homomorphicEv addV add && homomorphicEv mul mul && homomorphicEv sub sub
    && prop_operator_commutativity eq (sclV c) (mul c) atV x
  where homomorphicEv o1 o2 = prop_homomorphism eq o1 o2 atV x y
        atV                 = evaluate v


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
propDerivationProduct v1 v2 f g =
    ev (add (mul g (d f)) (mul f (d g))) == (ev . d) (mul f g)
  where ev = evaluate v1
        d  = derive v2

prop_derivation_product :: Vector Rational -> Vector Rational
                        -> Polynomial Rational -> Polynomial Rational
                        -> Bool
prop_derivation_product = propDerivationProduct

prop_derivation_productD :: Vector Double -> Vector Double
                         -> Polynomial Double -> Polynomial Double
                         -> Bool
prop_derivation_productD v1 v2 f g =
    ev (add (mul g (d f)) (mul f (d g))) `eqNum` (ev . d) (mul f g)
  where ev = evaluate v1
        d  = derive v2



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

prop_derivation_product_rational :: Vector Rational -> Vector Rational
                                 -> Rational
                                 -> Polynomial Rational -> Polynomial Rational
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
        oneLists    = map (map fromInt) (sumRLists (k+1) 1)

--------------------------------------------------------------------------------
-- Gradients of Barycentric Coordinates
--------------------------------------------------------------------------------

-- Helper function to create list of vector with 1.0 as first component
d0vectors :: Field a => Int -> [Vector a]
d0vectors n = [fromList $ [mulId]
                 ++ (replicate (i-1) addId)
                 ++ [addInv mulId]
                 ++ (replicate (n - i) addId) | i <- [1..n]]

-- Local gradients of barycentric coordinates, i.e. taken w.r.t the barycentric
-- coordinates themselves. Here we ensure that for a random simplex, if we move
-- from one corner a to corner b (in barycentric coordinates) the multiplication
-- of the  different vector with jacobian yields the corner b
-- (also in barycentric coordinates). This is equivalent to the vectors
-- [x_0, ..., x_n] with x_0 = 1 and x_i = -1 for any i > 0 being eigenvectors of
-- the gradient matrix.
prop_local_gradients :: Simplex (Vector Double) -> Bool
prop_local_gradients t = all (\v -> and $ zipWith eqNum (toList ((mult grads v)::Vector Double)) (toList v)) vs
  where grads     = localBarycentricGradients t
        vs        = d0vectors n :: [Vector Double]
        mult vs v = fromList [dot w v | w <- vs]
        n         = topologicalDimension t

return []
testPolynomial = $quickCheckWithAll
