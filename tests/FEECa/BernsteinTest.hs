{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module FEECa.BernsteinTest where

import Control.Monad

import FEECa.Internal.Simplex
import FEECa.Internal.Vector
import FEECa.Internal.Spaces

import FEECa.Utility.Utility                ( eqNum )

import qualified FEECa.Polynomial     as P ( degree )
import FEECa.Bernstein

import Properties
import FEECa.Utility.Test
import qualified FEECa.PolynomialTest as PT ( n, arbitraryPolynomial, propArithmetic )

import qualified Test.QuickCheck      as Q
import Test.QuickCheck                      ( (==>), property, quickCheckAll )


--------------------------------------------------------------------------------
-- Random Bernstein Polynomials
--------------------------------------------------------------------------------

-- | Generate random simplex of dimesion n = 8.
{- XXX: a second instance (vs the one in PT) and moreover not used in this
        module? (arbitrarySimplex used directly)
instance (EuclideanSpace v, Q.Arbitrary v) => Q.Arbitrary (Simplex v) where
    arbitrary = arbitrarySimplex n `Q.suchThat` ((addId /=) . volume)
-}

-- | Generate random Bernstein polynomial in n dimensions.
instance (EuclideanSpace v, r ~ Scalar v, Q.Arbitrary r, Q.Arbitrary v)
        => Q.Arbitrary (BernsteinPolynomial v r) where
  arbitrary = Q.frequency [ (3,arbitraryBernstein PT.n), (1,arbitraryConstant) ]

arbitraryBernstein :: (EuclideanSpace v, r ~ Scalar v,
                       Q.Arbitrary v, Q.Arbitrary r, Ring r)
                   => Int -> Q.Gen (BernsteinPolynomial v r)
arbitraryBernstein n =
  liftM2 Bernstein (arbitrarySimplex n) (PT.arbitraryPolynomial (n+1))

arbitraryConstant :: Q.Arbitrary r
                  => Q.Gen (BernsteinPolynomial v r)
arbitraryConstant = liftM Constant Q.arbitrary


type Bernstein = BernsteinPolynomial (Vector Double) Double

--------------------------------------------------------------------------------
-- Properties of arithmetic on Bernstein polynomials. Addition, subtraction and
-- multiplication must commute with the evaluate operator.
--------------------------------------------------------------------------------

prop_arithmetic :: Bernstein -> Bernstein -> Vector Double -> Double
                -> Q.Property
prop_arithmetic b1@(Bernstein t _) b2 v c =
  volume t > 0 ==> PT.propArithmetic eqNum b1 (redefine t b2) v c
prop_arithmetic b1 b2 v c =
  property $ PT.propArithmetic eqNum b1 b2 v c


--------------------------------------------------------------------------------
-- Integration of Bernstein Polynomials
--------------------------------------------------------------------------------

main = Q.quickCheck prop_integration
-- Test closed-form integration against general numeric integration.
prop_integration :: Bernstein -> Q.Property
prop_integration b@(Bernstein t p)
    | r > 0     = volume t > 0 ==> eqNum (integrate r t b) (integrateBernstein b)
    | otherwise = property True
  where r = P.degree p
prop_integration _ = property True

-- Linearity
-- TODO: change the generator to first generate one simplex, then two Bernstein on that simplex
-- prop_integration_linear c b1 b2  =  consistent b1 b2 ==>

prop_integration_linear :: Double -> Bernstein -> Bernstein -> Q.Property
prop_integration_linear c b1@(Bernstein t _) b2 =
    volume t > 0 ==> prop_linearity eqNum integrateBernstein c b1 b2'
  where b2' = redefine t b2
prop_integration_linear c b1 b2@(Bernstein t _) =
    volume t > 0 ==> prop_linearity eqNum integrateBernstein c b1' b2
  where b1' = redefine t b1
prop_integration_linear c b1@(Constant _) b2@(Constant _) = property True
    -- property $ prop_linearity eqNum integrateBernstein c b1 b2
-- TODO: Just True?? This should check remaining cases!! (constant?)
prop_integration_linear c b1 b2 = property False -- this should not happen


--------------------------------------------------------------------------------
-- Derivation of Bernstein Polynomials
--------------------------------------------------------------------------------

-- Linearity
prop_derivation_linear :: Vector Double -> Vector Double -> Double
                       -> Bernstein -> Bernstein
                       -> Bool
prop_derivation_linear v1 v2 c b1@(Bernstein t _) b2 =
    (prop_linearity eqNum (evaluate v1 . derive v2) c b1 b2')
  where b2' = redefine t b2
prop_derivation_linear v1 v2 c b1 b2 =
    (prop_linearity eqNum (evaluate v2 . derive v2) c b1 b2)


-- Product rule
-- TODO: Catch singular simplices!
prop_derivation_product :: Vector Double -> Vector Double
                        -> Bernstein -> Bernstein
                        -> Bool
prop_derivation_product v1 v2 b1@(Bernstein t _) b2 =
    eqNum (evaluate v1 (add (mul db1 b2') (mul db2 b1)))
          (evaluate v1 (derive v2 (mul b1 b2')))
  where b2' = redefine t b2
        db1 = derive v2 b1
        db2 = derive v2 b2'
prop_derivation_product v1 v2 b1 b2 =
    eqNum (evaluate v1 (add (mul db1 b2) (mul db2 b1)))
          (evaluate v1 (derive v2 (mul b1 b2)))
  where db1 = derive v2 b1
        db2 = derive v2 b2



--------------------------------------------------------------------------------

return []
testBernstein = $quickCheckAll

--------------------------------------------------------------------------------
