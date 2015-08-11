{-# LANGUAGE UndecidableInstances #-}

module FEEC.BernsteinTest where

import FEEC.Bernstein
import qualified FEEC.Internal.MultiIndex as MI
import FEEC.Internal.Simplex
import FEEC.Internal.SimplexTest
import FEEC.Internal.Vector
import FEEC.Internal.Spaces
import FEEC.Utility.Utility
import Properties
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Property as Prop
import qualified FEEC.Polynomial as P
import qualified FEEC.PolynomialTest as PT

--------------------------------------------------------------------------------
-- Random Bernstein Polynomials
--------------------------------------------------------------------------------

n = 8

instance (Ring r, Q.Arbitrary r, Q.Arbitrary v)
    => Q.Arbitrary (BernsteinPolynomial v r) where
    arbitrary = Q.oneof [ arbitraryBernstein PT.n, arbitraryConstant ]

arbitraryBernstein :: (Q.Arbitrary v, Q.Arbitrary r, Ring r)
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
                -> Bool
prop_arithmetic b1@(Bernstein t1 p1) (Bernstein t2 p2) v c
    = PT.prop_arithmetic eqNum b1 b2' v c
    where b2' = (Bernstein t1 p2)
prop_arithmetic b1 b2 v c = PT.prop_arithmetic eqNum b1 b2 v c

--------------------------------------------------------------------------------
-- Integration of Bernstein Polynomials
--------------------------------------------------------------------------------

-- Test closed-form integration against general numeric integration.
prop_integration :: Bernstein -> Bool
prop_integration b@(Bernstein t p)
    | r > 0 = eqNum (integrate r t b) (integrateBernstein b)
    | otherwise = True
    where r = P.degree p
prop_integration _ = True

-- Linearity
prop_integration_linear :: Double -> Bernstein -> Bernstein -> Bool
prop_integration_linear c b1@(Bernstein t1 _) b2 =
    prop_linearity eqNum integrateBernstein c b1 b2'
    where b2' = redefine t1 b2
prop_integration_linear c b1 b2@(Bernstein t1 _) =
    prop_linearity eqNum integrateBernstein c b1' b2
    where b1' = redefine t1 b1
prop_integration_linear c b1 b2 = True

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
    prop_linearity eqNum ((evaluate v1) . (derive v2)) c b1 b2'
    where b2' = redefine t1 b2
prop_derivation_linear v1 v2 c b1 b2 =
    prop_linearity eqNum ((evaluate v2) . (derive v2)) c b1 b2

-- Product rule
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

v1 = vector [2.23058424950095,-1.3759080736803717]
v2 = vector [2.6058896559591385,30.268642316887533]
t1 = simplex [vector [-3.050661050751067,1.445959300774541],vector [5.637624974734768,-4.118531033824756], vector [-1.9035009617473688,-2.7304505972438835]]
b1 :: Bernstein
b1 = polynomial t1 [(-2.971128240435243, MI.multiIndex [0,0,2])]
b2 :: Bernstein
b2 = polynomial t1 [(3.668944588464215, MI.multiIndex [3,1,6])]
v :: Vector Double
v = vector [0,1]
t :: Simplex (Vector Double)
t = simplex [vector [0,0], vector [0.5,0.5], vector [0.5,-0.5]] 
b :: Bernstein
b = polynomial t [(1.0,MI.multiIndex [])]
bb :: Bernstein
bb = polynomial t [(1.0,MI.multiIndex [0,1,1])]
