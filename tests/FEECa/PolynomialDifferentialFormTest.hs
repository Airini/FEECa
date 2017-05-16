{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module FEECa.PolynomialDifferentialFormTest where

import Data.Maybe     ( fromJust )
import Control.Monad  ( liftM, liftM2 )

import FEECa.Internal.Form
import FEECa.Internal.Simplex
import FEECa.Internal.Spaces
import FEECa.Internal.Vector

import FEECa.Polynomial
import FEECa.PolynomialDifferentialForm
import qualified FEECa.Bernstein                  as B
import qualified FEECa.PolynomialDifferentialForm as DF
import qualified FEECa.Internal.Spaces            as S

import FEECa.Utility.Combinatorics
import FEECa.Utility.Print
import FEECa.Utility.Utility

import FEECa.Utility.Test
import FEECa.BernsteinTest
import qualified FEECa.PolynomialTest as PT ( n )

import qualified Test.QuickCheck      as Q
import           Test.QuickCheck            ( arbitrary, (==>), Property )


--------------------------------------------------------------------------------
-- Random Simplices
--------------------------------------------------------------------------------

arbitraryConstituents :: (Q.Arbitrary a, Field a)
                      => Int -> Int -> Q.Gen [(BernsteinPolynomial a, [Int])]
arbitraryConstituents k n = do
  t  <- arbitrarySubsimplex k n
  let b  = arbitraryBernstein n
      c  = Q.vectorOf k $ (`mod` (k+1)) `liftM` arbitrary
      bs = Q.listOf1 (B.redefine t `liftM` b)
      cs = Q.listOf1 c
  liftM2 zip bs cs


arbitraryForm :: (Q.Arbitrary a, Field a)
              => Int -> Int -> Q.Gen (DifferentialForm a)
arbitraryForm k n = liftM (Form k n) (arbitraryConstituents k n)


instance (Field a, Q.Arbitrary a) => Q.Arbitrary (DifferentialForm a) where
  arbitrary = do
    k <- (`mod` PT.n) `liftM` arbitrary
    arbitraryForm (k+1) PT.n

arbitraryAltForm :: (Q.Arbitrary a, Field a)
                 => Int -> Int -> Q.Gen (Form a)
arbitraryAltForm k n = do
  let c  = Q.vectorOf k $ (`mod`(k+1)) `liftM` arbitrary 
      as = Q.listOf1 arbitrary
      cs = Q.listOf1 c
  liftM (Form k n) (liftM2 zip as cs)

instance Q.Arbitrary (Form Double) where
  arbitrary = arbitraryAltForm 3 5


--------------------------------------------------------------------------------
-- Apply
--------------------------------------------------------------------------------

prop_proj :: Simplex (Vector Double) -> Bool
prop_proj t = and [ dl i == vector [omOnj i j | j <- [0..n-1]] | i <- [0..n] ]
  where dl        = barycentricGradient t
        omOnj i j = refine (B.proj t) (omega i) [S.unitVector n j]
        omega i   = oneForm i n
        n         = topologicalDimension t

-- prop_alt_inner :: Double
--                -> Form Double
--                -> Double
--                -> Bool
-- prop_alt_inner c omega@(Form k n cs) eta =
--     (S.inner omega omega > 0 || b `eqNum` 0.0)
--     && (S.inner omega eta `eqNum` S.inner eta' omega)
--     -- && (DF.inner (sclV cc omega) eta) `eqNum` (DF.inner omega (sclV cc eta))
--     where b = apply omega (spanningVectors t)
-- -- cc = B.constant c

--------------------------------------------------------------------------------
-- Volume Form
--------------------------------------------------------------------------------

-- | Check that lambda_1 /\ ... /\ lambda_n ( v_1, ..., v_n) == 1.0 as stated
-- | on p. 44 in Arnold, Falk, Winther.
prop_volume_form :: Simplex (Vector Double) -> Vector Double -> Property
prop_volume_form t v =
    volume t > 0 ==> evaluate v b `eqNum` mulId
  where b     = DF.apply omega vs
        omega = Form n n [(B.constant t mulId, [1..n])]
        n     = topologicalDimension t
        vs    = spanningVectors t

--------------------------------------------------------------------------------
-- Integration
--------------------------------------------------------------------------------

-- | Formula (4.2) in Arndol, Falk, Winther.
prop_integral :: Simplex (Vector Double) -> Property
prop_integral t =
    volume t > 0 ==> all (nfac `eqNum`) [DF.integrate t (omega i) | i <- [1..n]]
  where omega i = Form n n [(lambda i, [1..n])]
        lambda  = B.barycentricCoordinate t
        nfac    = 1.0 / factorial (n + 1)
        n       = topologicalDimension t


--------------------------------------------------------------------------------
-- Inner Product
--------------------------------------------------------------------------------

redefine :: Simplex (Vector Double) -> DifferentialForm Double -> DifferentialForm Double
redefine t omega@(Form k n cs) = Form k n (map redefine' cs)
  where redefine' (b,c) = (B.redefine t b, c)


prop_inner :: Double -> DifferentialForm Double -> Property
prop_inner c omega@(Form k n _) =
    Q.forAll (redefine t `liftM` arbitraryForm k n) $ \eta ->
      let innerP = op eta
          normE  = op2 eta
          op2    = (`DF.inner` eta)
      in
          ( normO > 0 || S.inner b b `eqNum` 0.0 )
       && ( (innerP `eqNum` op' eta && op2 (sclV cp omega) `eqNum` mul c innerP)
            || normO `eqNum` 0.0 || normE `eqNum` 0.0 )
  where op    = DF.inner omega
        normO = op omega
        op'   = (`DF.inner` omega)
        b     = DF.apply omega (spanningVectors t)
        t     = fromJust (findSimplex omega)
        cp    = B.constant t c
        -- eta2   = DF.inner eta eta  -- XXX: or with eta' ?


return []
testDifferentialForm = $quickCheckWithAll

