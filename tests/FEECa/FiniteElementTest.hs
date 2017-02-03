{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module FEECa.FiniteElementTest where


import Control.Monad  ( liftM )

import FEECa.Internal.Form hiding ( inner )
import FEECa.Internal.Spaces
import qualified FEECa.Internal.MultiIndex  as MI
import qualified FEECa.Internal.Vector      as V
import qualified FEECa.Internal.Simplex     as S

import FEECa.Utility.Combinatorics
import FEECa.Utility.Print
import FEECa.Utility.Utility

import FEECa.Bernstein
import FEECa.FiniteElementSpace
import qualified FEECa.Polynomial                 as P
import qualified FEECa.PolynomialDifferentialForm as DF

import FEECa.Utility.Test
import FEECa.Internal.SimplexTest

import qualified Test.QuickCheck                 as Q
import Test.QuickCheck (quickCheckAll, (==>))

import qualified Numeric.LinearAlgebra.HMatrix    as M


--------------------------------------------------------------------------------
-- Random Finite Element Space
--------------------------------------------------------------------------------

max_n = 3
max_r = 3
max_k = 3


arbitraryPL :: (Int -> Int -> Simplex -> FiniteElementSpace)
            -> Q.Gen FiniteElementSpace
arbitraryPL cpl = do
  [n,r,k] <- mapM Q.choose [(1,max_n), (0,max_r), (0,max_k)]
  liftM (cpl r k) (arbitrarySimplex n)

arbitraryPrLk :: Q.Gen FiniteElementSpace
arbitraryPrLk = arbitraryPL PrLk

arbitraryPrmLk :: Q.Gen FiniteElementSpace
arbitraryPrmLk = arbitraryPL PrmLk


instance Q.Arbitrary FiniteElementSpace where
  arbitrary = arbitraryPrmLk

n = 4


--------------------------------------------------------------------------------
-- Whitney Forms
--------------------------------------------------------------------------------

data WhitneyTest = WhitneyTest Simplex
  deriving Show

instance Q.Arbitrary WhitneyTest where
  arbitrary = do k <- Q.choose (1,n)
                 liftM WhitneyTest (arbitrarySubsimplex k n)

prop_whitney_integral :: WhitneyTest -> Bool
prop_whitney_integral (WhitneyTest t) =
    abs (DF.integrate t (whitneyForm t [0..k]))
      `eqNum` (1 / fromInt (factorial k))
  where k = S.topologicalDimension t


--------------------------------------------------------------------------------
-- Psi Forms
--------------------------------------------------------------------------------

data PsiTest = PsiTest Simplex Simplex MI.MultiIndex Vector
  deriving Show

instance Q.Arbitrary PsiTest where
  arbitrary = do
    k <- Q.choose (1,n)
    t <- arbitrarySimplex n
    f <- Q.elements $ S.subsimplices t k
    r <- Q.choose (0,10)
    mi <- MI.extend n (S.sigma f) <$> arbitraryMI (k+1) r
    liftM (PsiTest t f mi) (arbitraryVector n)

prop_psi :: PsiTest -> Q.Property
prop_psi (PsiTest t f mi v) =
    MI.degree mi > 0 ==> all (eqNum 0.0 . evaluate v) ps
  where ps  = [DF.apply (psi' t f mi i) [l] | i <- is, l <- axs]
        axs = projectionAxes t f mi
        is  = S.sigma f
        k   = S.topologicalDimension f

convexCombination :: Simplex -> MI.MultiIndex -> Vector
convexCombination t mi = foldl addV zero (zipWith sclV mi' vs)
  where vs    = S.vertices t
        zero  = zeroV (head vs)
        (l,r) = (MI.toList mi, fromInt $ MI.degree mi)
        mi'   = [ let i = fromInt (l !! s)
                  in i / r  -- XXX: mulInv???...
                | s <- S.sigma t]

projectionAxes :: Simplex -> Simplex -> MI.MultiIndex -> [Vector]
projectionAxes t f mi = map (subV xmi) (S.complement t f)
  where xmi = convexCombination f mi

prop_basis :: FiniteElementSpace -> Q.Property
prop_basis s =
    length bs > 0 ==> length bs == dim s && linearIndependent DF.inner bs
  where bs  = basis s
        -- XXX: why the normalisation?? ... removed for now, seems unnecessary
        --      ==> we use bs instead
        --bs' = map (\x -> fmap (sclV (mulInv (sqrt (DF.inner x x)))) x) bs
        --n   = vspaceDim s


--------------------------------------------------------------------------------
-- PrmLk Form associated to a face.
--------------------------------------------------------------------------------

linearIndependent :: VectorSpace v => (v -> v -> Double) -> [v] -> Bool
linearIndependent f bs = M.rank mat == n
  where es  = M.eigenvaluesSH' mat
        -- TODO: changed eigenvaluesSH to eigenvaluesSH' for loading; check!
        mat = M.matrix n [ f omega eta | omega <- bs, eta <- bs ]
        n   = length bs


return []
testFiniteElement = $quickCheckAll


space = PrmLk 3 0 (S.referenceSimplex 3)

