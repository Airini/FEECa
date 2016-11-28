{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module FEECa.FiniteElementTest where

import Control.Monad
import FEECa.Utility.Print
import FEECa.FiniteElementSpace
import FEECa.Internal.SimplexTest
import FEECa.Internal.Spaces
import qualified FEECa.Internal.MultiIndex as MI
import qualified FEECa.Internal.Vector as V
import FEECa.Internal.Form hiding (inner)
import FEECa.Utility.Combinatorics
import FEECa.Utility.Test
import FEECa.Utility.Utility
import FEECa.Bernstein
import Control.Applicative
import qualified FEECa.Internal.Simplex           as S
import qualified FEECa.PolynomialDifferentialForm as D
import qualified FEECa.Polynomial                 as P
import qualified Numeric.LinearAlgebra.HMatrix   as M
import qualified Test.QuickCheck                 as Q
import qualified Test.QuickCheck.Property        as Prop
import Debug.Trace

--------------------------------------------------------------------------------
-- Random Finite Element Space
--------------------------------------------------------------------------------

max_n = 3
max_r = 3
max_k = 3

arbitraryPrLk :: Q.Gen FiniteElementSpace
arbitraryPrLk = do n <- Q.choose (1, max_n)
                   r <- Q.choose (0, max_r)
                   k <- Q.choose (0, max_k)
                   t <- arbitrarySimplex n
                   return $ PrLk r k t

arbitraryPrmLk :: Q.Gen FiniteElementSpace
arbitraryPrmLk = do n <- Q.choose (1, max_n)
                    r <- Q.choose (0, max_r)
                    k <- Q.choose (0, max_k)
                    t <- arbitrarySimplex n
                    return $ PrmLk r k t

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
                   t <- arbitrarySubsimplex k n
                   return (WhitneyTest t)

prop_whitney_integral :: WhitneyTest -> Bool
prop_whitney_integral (WhitneyTest t) =
    abs(D.integrate t (whitneyForm t [0..k])) `eqNum` (1 / fromIntegral (factorial k))
       where k = S.topologicalDimension t

--------------------------------------------------------------------------------
-- Psi Forms
--------------------------------------------------------------------------------

data PsiTest = PsiTest Simplex Simplex MI.MultiIndex Vector
                 deriving Show
instance Q.Arbitrary PsiTest where
    arbitrary = do k <- Q.choose (1,n)
                   t <- arbitrarySimplex n
                   f <- Q.elements $ S.subsimplices t k
                   r <- Q.choose (0,10)
                   mi <- MI.extend n (S.sigma f) <$> arbitraryMI (k+1) r
                   v <- arbitraryVector n
                   return (PsiTest t f mi v)

prop_psi :: PsiTest -> Q.Property
prop_psi (PsiTest t f mi v) =
    MI.degree mi > 0 Q.==> all (0.0 `eqNum`) $ map (evaluate v) ps
    where ps  = [D.apply (psi' t f mi i) [l] | i <- is, l <- axs]
          axs = projectionAxes t f mi
          is  = S.sigma f
          k   = S.topologicalDimension f

convexCombination :: Simplex -> MI.MultiIndex -> Vector
convexCombination t mi = foldl addV zero (zipWith sclV mi' vs)
    where vs   = S.vertices t
          zero = zeroV (head vs)
          mi'  = map  (flip (/) r . fromInteger) mi''
          mi'' = [MI.toList mi !! s | s <- S.sigma t]
          r    = fromInteger $ MI.degree mi

projectionAxes :: Simplex -> Simplex -> MI.MultiIndex -> [Vector]
projectionAxes t f mi = map (subV xmi) (S.complement t f)
    where xmi = convexCombination f mi

prop_basis :: FiniteElementSpace -> Q.Property
prop_basis s = length bs > 0 Q.==>
               length bs == dim s && linearIndependent D.inner bs'
    where bs = basis s
          bs' = map (\ x -> fmap (sclV (mulInv (sqrt (D.inner x x)))) x) bs
          n = vspaceDim s

--------------------------------------------------------------------------------
-- PrmLk Form associated to a face.
--------------------------------------------------------------------------------

linearIndependent ::  VectorSpace v => (v -> v -> Double) -> [v] -> Bool
linearIndependent f bs = M.rank mat == length bs
    where es = M.eigenvaluesSH' mat
          -- TODO: changed eigenvaluesSH to eigenvaluesSH' for loading; check!
          mat = M.matrix n [ f omega eta | omega <- bs, eta <- bs ]
          n = length bs


space = PrmLk 3 0 (S.referenceSimplex 3)
