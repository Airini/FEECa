{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module FEEC.FiniteElementTest where

import Control.Monad
import FEEC.Utility.Print
import FEEC.FiniteElementSpace
import FEEC.Internal.SimplexTest
import FEEC.Internal.Spaces
import qualified FEEC.Internal.MultiIndex as MI
import qualified FEEC.Internal.Vector as V
import FEEC.Internal.Form hiding (inner)
import FEEC.Utility.Combinatorics
import FEEC.Utility.Test
import FEEC.Utility.Utility
import FEEC.Bernstein
import Control.Applicative
import qualified FEEC.Internal.Simplex           as S
import qualified FEEC.PolynomialDifferentialForm as D
import qualified FEEC.Polynomial                 as P
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
                   k <- Q.choose (1, max_k)
                   t <- arbitrarySimplex n
                   return $ PrLk r k t

arbitraryPrmLk :: Q.Gen FiniteElementSpace
arbitraryPrmLk = do n <- Q.choose (1, max_n)
                    r <- Q.choose (0, max_r)
                    k <- Q.choose (1, max_k)
                    t <- arbitrarySimplex n
                    return $ PrmLk r k t

instance Q.Arbitrary FiniteElementSpace where
    arbitrary = arbitraryPrLk

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
                   mi <- fmap (MI.extend n (S.sigma f)) $ arbitraryMI (k+1) r
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
          mi'  = map  (flip (/) r . fromInteger) $ mi''
          mi'' = [(MI.toList mi) !! s | s <- (S.sigma t)]
          r    = fromInteger $ MI.degree mi

projectionAxes :: Simplex -> Simplex -> MI.MultiIndex -> [Vector]
projectionAxes t f mi = map (subV xmi) (S.complement t f)
    where xmi = convexCombination f mi

f  = undefined
t  = undefined
mi = undefined
v  = undefined

axs = map (flip (:) []) $ projectionAxes t f mi
ps  = psi' t f mi 2
--ps' = evaluate v (D.apply ps (axs!!0))

prop_basis :: FiniteElementSpace -> Q.Property
prop_basis s = length bs > 0 Q.==>
               (length bs) == (dim s) && linearIndependent D.inner bs'
    where bs = basis s
          bs' = map (\ x -> fmap (sclV (mulInv (sqrt (D.inner x x)))) x) bs
          n = vspaceDim s

--------------------------------------------------------------------------------
-- PrmLk Form associated to a face.
--------------------------------------------------------------------------------

linearIndependent ::  VectorSpace v => (v -> v -> Double) -> [v] -> Bool
linearIndependent f bs = (M.rank mat) > 0
    where es = M.eigenvaluesSH mat
          mat = M.matrix n $ [ f omega eta | omega <- bs, eta <- bs ]
          n = length bs

ws = [ V.vector [1.0, 0.0, 0.0], V.vector [0.0, 1.0, 0.0], V.vector [0.0, 1.0, 0.0] ]



space = PrLk 2 1 (S.Simplex {S.sigma = [0,1,2], S.vertices = [V.Vector {V.components = [0.9636521027274948,0.8119987749173205]},V.Vector {V.components = [-0.45272955850518726,-0.6121936713301906]},V.Vector {V.components = [0.18658420888905336,-0.9970618163249919]}]})
tt = S.Simplex {S.sigma = [0,1,2], S.vertices = [V.Vector {V.components = [-2.784907123707038,-1.6532876554872296]},V.Vector {V.components = [-3.4615647789358106e-2,-1.1346265590395175]},V.Vector {V.components = [0.38249423114777337,5.421970782899612]}]}
bs  = basis space
mat = [D.inner omega eta | omega <- bs, eta <- bs]

omega = whitneyForm f [0..(S.topologicalDimension f)]

vs = S.vertices f

--rs = [[evaluate v b | v <- vs ] | b <- bs]
