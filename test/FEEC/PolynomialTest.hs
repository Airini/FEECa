{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module PolynomialTest where


import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Internal.Vector
import FEEC.Polynomial
import FEEC.Internal.Spaces
import FEEC.Utility.Test
import qualified Test.QuickCheck as Q
import Properties
import FEEC.Internal.Simplex

-- Dimension of the space to be tested. Must be greater than zero.
n :: Int
n = 2

-- Generate a random polynomial of dimension n as fixed by the above parameter.
-- The Polynomial may be of type Constant or Polynomial. If it is of type
-- Polynomial, degree the degree is chosen between 0 and 10 and the constants
-- are chosen randomly.
instance (Ring r, Q.Arbitrary r) => Q.Arbitrary (Polynomial r)
    where arbitrary = Q.oneof [arbitraryPolynomial, arbitraryConstant]

arbitraryPolynomial :: (Ring r, Q.Arbitrary r) => Q.Gen (Polynomial r)
arbitraryPolynomial = do r <- Q.choose (0,10)
                         mis <- Q.listOf (arbitraryMI n r)
                         cs <- Q.listOf Q.arbitrary
                         return $ polynomial (zip cs mis)

arbitraryConstant :: (Ring r, Q.Arbitrary r) => Q.Gen (Polynomial r)
arbitraryConstant = do c <- Q.arbitrary
                       return $ constant c

-- Generate random vectors of dimension n, so that they can be used to evaluate
-- the randomly generated polynomials.
instance Field f => Q.Arbitrary (Vector f) where
    arbitrary = do l <- Q.vector n
                   return $ vector (map fromDouble l)


-- Abstract propreties of arithmetic on polynomials. Addition, Subtraction and
-- multiplication of polynomials must commute with the evaluate operator.
prop_arithmetic :: (EuclideanSpace v r, Q.Arbitrary v, Q.Arbitrary r)
                => Polynomial r
                -> Polynomial r
                -> v
                -> r
                -> Bool
prop_arithmetic p1 p2 v c = prop_operator2_commutativity add add (evaluate v) p1 p2
                            && prop_operator2_commutativity mul mul (evaluate v) p1 p2
                            && prop_operator2_commutativity sub sub (evaluate v) p1 p2
                            && prop_operator_commutativity (sclV c) (mul c) (evaluate v) p1

-- Concrete arithmetic properties for polynomials defined over rationals. 
prop_arithmetic_rf :: Polynomial Rational
                   -> Polynomial Rational
                   -> Vector Rational
                   -> Rational
                   -> Bool
prop_arithmetic_rf = prop_arithmetic

