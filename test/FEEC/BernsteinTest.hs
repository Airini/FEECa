{-# LANGUAGE TypeFamilies #-}

module FEEC.BernsteinTest where

import Control.Monad
import FEEC.Bernstein
import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Internal.Simplex
import FEEC.Internal.Vector
import FEEC.Internal.Spaces
import FEEC.Utility.Utility
import FEEC.PolynomialTest(n)
import FEEC.Utility.Print
import Properties
import Test.QuickCheck.Property(Property, (==>), property)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Property as Prop
import qualified FEEC.Polynomial as P
import qualified FEEC.PolynomialTest as PT

--------------------------------------------------------------------------------
-- Random Bernstein Polynomials
--------------------------------------------------------------------------------
arbitraryVector :: EuclideanSpace v => Int -> Q.Gen v
arbitraryVector n = do cs <- Q.vector n
                       return (fromDouble' cs)

-- | Generate a random simplex of given dimension.
arbitrarySimplex :: (EuclideanSpace v, Q.Arbitrary v)
                 => Int -> Q.Gen (Simplex v)
arbitrarySimplex n =  t `Q.suchThat` ((addId /=) . volume)
                     where t = liftM simplex vs
                           vs = Q.vectorOf (n+1) (arbitraryVector n)

-- | Generate random simplex of dimesion n = 8.
instance (EuclideanSpace v, Q.Arbitrary v) => Q.Arbitrary (Simplex v) where
    arbitrary = arbitrarySimplex n `Q.suchThat` (addId /=) . volume

-- | Generate random Bernstein polynomial in n dimensions.
instance (EuclideanSpace v, r ~ Scalar v, Q.Arbitrary r, Q.Arbitrary v)
    => Q.Arbitrary (BernsteinPolynomial v r) where
    arbitrary = Q.oneof [ arbitraryBernstein PT.n, arbitraryConstant ]

arbitraryBernstein :: (EuclideanSpace v, r ~ Scalar v,
                       Q.Arbitrary v, Q.Arbitrary r, Ring r)
                   => Int
                   -> Q.Gen (BernsteinPolynomial v r)
arbitraryBernstein n = do t <- arbitrarySimplex n
                          p <- PT.arbitraryPolynomial (n + 1)
                          return (Bernstein t p)

arbitraryConstant :: Q.Arbitrary r
                  => Q.Gen (BernsteinPolynomial v r)
arbitraryConstant = do c <- Q.arbitrary
                       return (Constant c)

--------------------------------------------------------------------------------
-- Properties of arithmetic on Bernstein polynomials. Addition, subtraction and
-- multiplication must commute with the evaluate operator.
--------------------------------------------------------------------------------

prop_arithmetic :: BernsteinPolynomial (Vector Double) Double
                -> BernsteinPolynomial (Vector Double) Double
                -> Vector Double
                -> Double
                -> Property
prop_arithmetic b1@(Bernstein t1 p1) (Bernstein t2 p2) v c
    = volume t1 > 0 ==> PT.prop_arithmetic eqNum b1 b2' v c
    where b2' = Bernstein t1 p2
prop_arithmetic b1@(Bernstein t1 p1) b2 v c
    = volume t1 > 0 ==> PT.prop_arithmetic eqNum b1 b2 v c
prop_arithmetic b1 b2 v c = property $ PT.prop_arithmetic eqNum b1 b2 v c


--------------------------------------------------------------------------------
-- Integration of Bernstein Polynomials
--------------------------------------------------------------------------------

-- Test closed-form integration against general numeric integration.
prop_integration :: Bernstein -> Property
prop_integration b@(Bernstein t p)
    | r > 0     = volume t > 0 ==> eqNum (integrate r t b) (integrateBernstein b)
    | otherwise = property True
    where r = P.degree p
prop_integration _ = property True

-- Linearity
prop_integration_linear :: Double -> Bernstein -> Bernstein -> Property
prop_integration_linear c b1@(Bernstein t1 _) b2 =
    volume t1 > 0 ==>
           prop_linearity eqNum integrateBernstein c b1 b2'
    where b2' = redefine t1 b2
prop_integration_linear c b1 b2@(Bernstein t1 _) =
    volume t1 > 0 ==>
           prop_linearity eqNum integrateBernstein c b1' b2
    where b1' = redefine t1 b1
prop_integration_linear c b1 b2 = property True

--------------------------------------------------------------------------------
-- Derivation of Bernstein Polynomials
--------------------------------------------------------------------------------

-- Linearity
prop_derivation_linear :: Vector Double
                       -> Vector Double
                       -> Double
                       -> Bernstein
                       -> Bernstein
                       -> Bool
prop_derivation_linear v1 v2 c b1@(Bernstein t1 _) b2 =
    prop_linearity eqNum (evaluate v1 . derive v2) c b1 b2'
  where b2' = redefine t1 b2
prop_derivation_linear v1 v2 c b1 b2 =
    prop_linearity eqNum (evaluate v2 . derive v2) c b1 b2


-- Product rule
-- TODO: Catch singular simplices!
prop_derivation_product :: Vector Double
                        -> Vector Double
                        -> Bernstein
                        -> Bernstein
                        -> Bool
prop_derivation_product v1 v2 b1@(Bernstein t _) b2 =
               eqNum (evaluate v1 (add (mul db1 b2') (mul db2' b1)))
                     (evaluate v1 (derive v2 (mul b1 b2')))
        where b2' = redefine t b2
              db1 = derive v2 b1
              db2' = derive v2 b2'
prop_derivation_product v1 v2 b1 b2 =
               eqNum (evaluate v1 (add (mul db1 b2) (mul db2 b1)))
                     (evaluate v1 (derive v2 (mul b1 b2)))
        where db1 = derive v2 b1
              db2 = derive v2 b2

type Bernstein = BernsteinPolynomial (Vector Double) Double

