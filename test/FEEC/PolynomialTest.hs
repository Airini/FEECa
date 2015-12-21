{-# LANGUAGE TypeFamilies #-}
module FEEC.PolynomialTest where


import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Internal.Vector
import FEEC.Polynomial
import FEEC.Internal.Spaces
import FEEC.TestUtility
import qualified Test.QuickCheck as Q
import Properties
import FEEC.Internal.Simplex

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

instance (Ring r, Q.Arbitrary r) => Q.Arbitrary (Polynomial r)
    where arbitrary = Q.oneof [arbitraryPolynomial n, arbitraryConstant]

arbitraryPolynomial :: (Ring r, Q.Arbitrary r) => Int -> Q.Gen (Polynomial r)
arbitraryPolynomial  n = do r <- Q.choose (0,10)
                            mis <- Q.listOf1 (arbitraryMI n r)
                            cs <- Q.listOf1 Q.arbitrary
                            return $ polynomial (zip (take 10 cs) (take 10 mis))

arbitraryConstant :: (Ring r, Q.Arbitrary r) => Q.Gen (Polynomial r)
arbitraryConstant = do c <- Q.arbitrary
                       return $ constant c

------------------------------------------------------------------------------
-- Generate random vectors of dimension n, so that they can be used to evaluate
-- the randomly generated polynomials.
------------------------------------------------------------------------------

instance Field f => Q.Arbitrary (Vector f) where
    arbitrary = do l <- Q.vector n
                   return $ vector (map fromDouble l)


------------------------------------------------------------------------------
-- Abstract propreties of arithmetic on polynomials. Addition, Subtraction and
-- multiplication of polynomials must commute with the evaluate operator.
------------------------------------------------------------------------------

prop_arithmetic :: (EuclideanSpace v, Function f v, VectorSpace f, Ring f,
                    r ~ Scalar v, r ~ Scalar f)
                => (r -> r -> Bool)
                -> f
                -> f
                -> v
                -> r
                -> Bool
prop_arithmetic eq f1 f2 v c =
    prop_operator2_commutativity eq addV add (evaluate v) f1 f2
    && prop_operator2_commutativity eq mul mul (evaluate v) f1 f2
    && prop_operator2_commutativity eq sub sub (evaluate v) f1 f2
    && prop_operator_commutativity eq (sclV c) (mul c) (evaluate v) f1

------------------------------------------------------------------------------
-- Concrete arithmetic properties for polynomials defined over rationals.
------------------------------------------------------------------------------

prop_arithmetic_rf :: Polynomial Rational
                   -> Polynomial Rational
                   -> Vector Rational
                   -> Rational
                   -> Bool
prop_arithmetic_rf = prop_arithmetic (==)

--------------------------------------------------------------------------------
-- Derivation of Polynomials
--------------------------------------------------------------------------------

-- Linearity
prop_derivation_linear :: (EuclideanSpace v, Function f v, VectorSpace f,
                          r ~ (Scalar v), r ~ (Scalar f))
                       => v
                       -> v
                       -> r
                       -> f
                       -> f
                       -> Bool
prop_derivation_linear v1 v2 c p1 p2 =
    prop_linearity (==) ((evaluate v2) . (derive v2)) c p1 p2

-- Product rule
prop_derivation_product :: (EuclideanSpace v, Function f v, Ring f)
                        => v
                        -> v
                        -> f
                        -> f
                        -> Bool
prop_derivation_product v1 v2 p1 p2 =
    (evaluate v1 (add (mul dp1 p2) (mul dp2 p1)))
    == (evaluate v1 (derive v2 (mul p1 p2)))
        where dp1 = derive v2 p1
              dp2 = derive v2 p2

-- Test for polynomials using exact arithmetic.
prop_arithmetic_rational :: Polynomial Rational
                         -> Polynomial Rational
                         -> Vector Rational
                         -> Rational
                         -> Bool
prop_arithmetic_rational = prop_arithmetic (==)

prop_derivation_linear_rational :: Vector Rational
                                -> Vector Rational
                                -> Rational
                                -> Polynomial Rational
                                -> Polynomial Rational
                                -> Bool
prop_derivation_linear_rational = prop_derivation_linear

prop_derivation_product_rational :: Vector Rational
                                 -> Vector Rational
                                 -> Rational
                                 -> Polynomial Rational
                                 -> Polynomial Rational
                                 -> Bool
prop_derivation_product_rational = prop_derivation_linear
