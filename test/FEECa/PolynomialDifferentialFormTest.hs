{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module FEECa.PolynomialDifferentialFormTest where

import Control.Applicative
import Data.Maybe
import Control.Monad
import FEECa.BernsteinTest
import FEECa.Internal.Form
import FEECa.Internal.Simplex
import FEECa.Internal.Spaces
import FEECa.Internal.Vector
import FEECa.PolynomialDifferentialForm
import FEECa.PolynomialTest(n)
import FEECa.Polynomial
import qualified FEECa.Bernstein                  as B
import qualified FEECa.Utility.Combinatorics      as C
import qualified FEECa.PolynomialDifferentialForm as D
import qualified FEECa.Internal.Spaces            as S
import FEECa.Utility.Combinatorics
import FEECa.Utility.Print
import FEECa.Utility.Utility

import Test.QuickCheck(Arbitrary, arbitrary, quickCheck, (==>), Property)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as Q

--------------------------------------------------------------------------------
-- Random Simplices
--------------------------------------------------------------------------------

-- | Generate a random k-subsimplex of given dimension n.
arbitrarySubsimplex :: (EuclideanSpace v, Arbitrary v)
                    => Int
                    -> Int
                    -> Q.Gen (Simplex v)
arbitrarySubsimplex k n = do t <- arbitrarySimplex n
                             Q.elements (subsimplices t k)


arbitraryConstituents :: (Q.Arbitrary a, Field a)
                        => Int
                        -> Int
                        -> Q.Gen [(BernsteinPolynomial a, [Int])]
arbitraryConstituents k n = do let b = arbitraryBernstein n
                                   c = Q.vectorOf k $ flip mod (k+1) `liftM` arbitrary 
                               t  <- arbitrarySubsimplex k n
                               bs <- Q.listOf1 b
                               cs <- Q.listOf1 c
                               return $ zip (map (B.redefine t) bs) cs

arbitraryForm :: (Q.Arbitrary a, Field a)
              => Int
              -> Int
              -> Q.Gen (DifferentialForm a)
arbitraryForm k n = do cs <- arbitraryConstituents k n
                       return $ Form k n cs

instance (Field a, Q.Arbitrary a) => Q.Arbitrary (DifferentialForm a) where
    arbitrary = arbitraryForm n n -- k <- ((flip mod) n) `liftM` arbitrary

arbitraryAltForm :: (Q.Arbitrary a, Field a)
                 => Int
                 -> Int
                 -> Q.Gen (Form a)
arbitraryAltForm k n = do let c = Q.vectorOf k $ flip mod (k+1) `liftM` arbitrary 
                          as <- Q.listOf1 arbitrary
                          cs <- Q.listOf1 c
                          return $ Form k n (zip as cs)

instance Q.Arbitrary (Form Double) where
    arbitrary = arbitraryAltForm 3 5

--------------------------------------------------------------------------------
-- Apply
--------------------------------------------------------------------------------

prop_proj :: Simplex (Vector Double) -> Bool
prop_proj t = and [vector [applyOmega i j | j <- [0..n-1]] == dlambda i | i <- [0..n]]
    where dlambda        = barycentricGradient t
          applyOmega i j = refine (B.proj t) (omega i) [S.unitVector n j]
          omega i        = Form 1 n [(1.0,[i])]
          n              = geometricalDimension t

-- prop_alt_inner :: Double
--                -> Form Double
--                -> Double
--                -> Bool
-- prop_alt_inner c omega@(Form k n cs) eta =
--     (S.inner omega omega > 0 || b `eqNum` 0.0)
--     && (S.inner omega eta `eqNum` S.inner eta' omega)
--     -- && (D.inner (sclV cc omega) eta) `eqNum` (D.inner omega (sclV cc eta))
--     where b = apply omega (spanningVectors t)
-- -- cc = B.constant c

--------------------------------------------------------------------------------
-- Volume Form
--------------------------------------------------------------------------------

-- | Check that lambda_1 /\ ... /\ lambda_n ( v_1, ..., v_n) == 1.0 as stated
-- | on p. 44 in Arnold, Falk, Winther.
prop_volume_form :: Simplex (Vector Double) -> Vector Double -> Property
prop_volume_form t v = volume t > 0 ==> evaluate v b `eqNum` 1.0
    where b = apply omega vs
          vs = spanningVectors t
          omega = Form n n [(B.redefine t mulId, [1..n])]
          n = topologicalDimension t

--------------------------------------------------------------------------------
-- Integration
--------------------------------------------------------------------------------

-- | Formula (4.2) in Arndol, Falk, Winther.
prop_integral :: Simplex (Vector Double) -> Property
prop_integral t = volume t > 0
                  ==> all (nfac `eqNum`) [D.integrate t (omega i) | i <- [1..n]]
    where omega i = Form n n [(lambda i, [1..n])]
          lambda  = B.barycentricCoordinate t
          nfac    = 1.0 / factorial (n + 1)
          n       = topologicalDimension t

--------------------------------------------------------------------------------
-- Inner Product
--------------------------------------------------------------------------------

redefine :: Simplex (Vector Double) -> DifferentialForm Double -> DifferentialForm Double
redefine t omega@(Form k n cs) = Form k n (map (redefine' t) cs)
    where redefine' t (b,c) = (B.redefine t b, c)


prop_inner :: Double
              -> DifferentialForm Double
              -> DifferentialForm Double
              -> Bool
prop_inner c omega@(Form k n cs) eta =
    (D.inner omega omega > 0 || S.inner b b `eqNum` 0.0)
    && ((D.inner omega eta' `eqNum` D.inner eta' omega &&
         D.inner (sclV cc omega) eta `eqNum` mul c (D.inner omega eta))
         || omega2 `eqNum` 0.0 || eta2 `eqNum` 0.0)
    where b    = apply omega (spanningVectors t)
          eta' = redefine (fromJust (findSimplex omega)) eta
          t    = fromJust (findSimplex omega)
          origin = zero n :: Vector Double
          omega2 = D.inner omega omega
          eta2   = D.inner eta eta
          cc = B.constant c
