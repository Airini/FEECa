{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module FEEC.FiniteElementTest where

import Control.Monad
import FEEC.Utility.Print
import FEEC.FiniteElementSpace
import FEEC.Internal.SimplexTest
import FEEC.Internal.Spaces
import qualified FEEC.Internal.Vector as V
import FEEC.Internal.Form hiding (inner)
import FEEC.Utility.Combinatorics
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
                   let t = S.referenceSimplex n
                   return $ PrLk r k t

arbitraryPrmLk :: Q.Gen FiniteElementSpace
arbitraryPrmLk = do n <- Q.choose (1, max_n)
                    r <- Q.choose (0, max_r)
                    k <- Q.choose (1, max_k)
                    let t = S.referenceSimplex n
                    return $ PrmLk r k t

instance Q.Arbitrary FiniteElementSpace where
    arbitrary = arbitraryPrmLk

--------------------------------------------------------------------------------
-- Random Finite Element Space
--------------------------------------------------------------------------------
n = 4

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

prop_basis :: FiniteElementSpace -> Q.Property
prop_basis s = length bs > 0 Q.==>
               (length bs) == (dim s) && linearIndependent D.inner bs'
    where bs = basis s
          bs' = map (\ x -> sclV (constant $ mulInv.sqrt $ (D.inner x x)) x) bs
          n = vspaceDim s

--------------------------------------------------------------------------------
-- PrmLk Form associated to a face.
--------------------------------------------------------------------------------


linearIndependent ::  VectorSpace v => (v -> v -> Double) -> [v] -> Bool
linearIndependent f bs = all (0 <) $ M.toList es
    where es = M.eigenvaluesSH mat
          mat = M.matrix n $ traceShowId [ f omega eta | omega <- bs, eta <- bs ]
          n = length bs

ws = [ V.vector [1.0, 0.0, 0.0], V.vector [0.0, 1.0, 0.0], V.vector [0.0, 1.0, 0.0] ]

t = S.referenceSimplex 4


space = PrmLk 2 1 $ S.referenceSimplex 2

bs  = basis space
bs' = map (\ x -> sclV (constant $ mulInv.sqrt $ (D.inner x x)) x) bs
mat = [D.inner omega eta | omega <- bs', eta <- bs']

omega = whitneyForm tt [0..(S.topologicalDimension tt)]

tt = S.subsimplex t 2 0
vs = S.vertices tt

--rs = [[evaluate v b | v <- vs ] | b <- bs]
