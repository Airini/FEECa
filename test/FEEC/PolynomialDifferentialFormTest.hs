{-# LANGUAGE FlexibleInstances #-}

module FEEC.PolynomialDifferentialFormTest where

import Data.Maybe
import Control.Monad
import FEEC.BernsteinTest
import FEEC.Internal.Form
import FEEC.Internal.Simplex
import FEEC.Internal.Spaces
import FEEC.Internal.Vector
import FEEC.PolynomialDifferentialForm
import FEEC.PolynomialTest(n)
import qualified FEEC.Bernstein                  as B
import qualified FEEC.Utility.Combinatorics      as C
import qualified FEEC.PolynomialDifferentialForm as D
import FEEC.Utility.Combinatorics
import FEEC.Utility.Print
import FEEC.Utility.Utility

import Test.QuickCheck(Arbitrary, arbitrary, quickCheck, (==>), Property)
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Gen as Q

--------------------------------------------------------------------------------
-- Random Simplices
--------------------------------------------------------------------------------

arbitraryConstituent :: (Q.Arbitrary a, Field a)
                        => Int
                        -> Int
                        -> Q.Gen (BernsteinPolynomial a, [Int])
arbitraryConstituent k n = do b <- arbitraryBernstein k
                              i <- ((flip mod) (n + 1)) `liftM` arbitrary
                              let l = C.unrank k n i
                              return (b,l)

arbitraryForm :: (Q.Arbitrary a, Field a)
              => Int
              -> Int
              -> Q.Gen (DifferentialForm a)
arbitraryForm k n = do cs <- Q.listOf1 (arbitraryConstituent k n)
                       return $ Form n k cs

instance (Field a, Q.Arbitrary a) => Q.Arbitrary (DifferentialForm a) where
    arbitrary = do k <- ((flip mod) (n + 1)) `liftM` arbitrary
                   arbitraryForm k n

--------------------------------------------------------------------------------
-- Volume Form
--------------------------------------------------------------------------------

-- | Check that lambda_1 /\ ... /\ lambda_n ( v_1, ..., v_n) == 1.0 as stated
-- | on p. 44 in Arnold, Falk, Winther.
prop_volume_form :: Simplex (Vector Double) -> (Vector Double) -> Property
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
    where omega i  = Form n n [(lambda i, [1..n])]
          lambda i = B.barycentricCoordinate t i
          nfac     = 1.0 / (factorial (n + 1))
          n        = topologicalDimension t

--------------------------------------------------------------------------------
-- Inner Product
--------------------------------------------------------------------------------

prop_inner :: DifferentialForm Double -> Bool
prop_inner omega@(Form k n cs) = D.inner omega omega > 0 ||
                                 evaluate origin b `eqNum` 0.0
    where b = apply omega (spanningVectors t)
          t = (fromJust (findSimplex omega))
          origin = (zero n) :: Vector Double

