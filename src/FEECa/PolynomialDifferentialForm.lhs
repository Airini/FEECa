\begin{code}

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FEECa.PolynomialDifferentialForm where

import Control.Applicative  ( liftA2 )
import Control.Monad        ( MonadPlus (..) )
import Data.Foldable  ( Foldable (..) )
import Data.Monoid    ( First (..) )
import Data.Maybe

import            FEECa.Internal.Vector
import            FEECa.Internal.Simplex
import qualified  FEECa.Bernstein             as B
import qualified  FEECa.Internal.Form         as F
import qualified  FEECa.DifferentialForm      as D
import qualified  FEECa.Polynomial            as P
import qualified  FEECa.Internal.Spaces       as S
import qualified  FEECa.Utility.Combinatorics as C
import qualified  FEECa.Internal.Vector       as V
import            FEECa.Utility.Utility(pairM)

type BernsteinPolynomial a = B.BernsteinPolynomial (Vector a) a
type DifferentialForm a    = D.DifferentialForm (BernsteinPolynomial a)

{-
instance  Show (DifferentialForm Double) where
  show omega = show $ printForm ("d" ++ lambda) "0" pPrint (F.terms omega)
-}

findSimplex :: DifferentialForm a -> Maybe (Simplex (Vector a))
findSimplex (F.Form _ _ cs) = getFirst $ foldMap (findSimplex' . fst) cs

findSimplex' :: BernsteinPolynomial a -> First (Simplex (Vector a))
findSimplex' (B.Bernstein t _)  = First (Just t)
findSimplex' _                  = First Nothing

tabulate :: (S.VectorSpace a, S.Scalar a ~ a)
         => [DifferentialForm a]
         -> [Vector a]
         -> [Simplex (Vector a)]
         -> [[a]]
tabulate bs vs fs = [ evalSeparately t b vs fs' | b <- bs ]
  where t   = fromJust $ findSimplex $ head bs
        fs' = map spanningVectors fs

apply :: (S.Field a, Show a)
      => DifferentialForm a -> [Vector a] -> BernsteinPolynomial a
apply (F.Form k n []) _ = S.addId
apply omega vs = {-# SCC "apply" #-} F.apply ds vs omega
  where t  = fromJust (findSimplex omega)
        ds = P.barycentricGradients t

evalSeparately :: (S.VectorSpace a, S.Scalar a ~ a)
               => Simplex (Vector a)
               -> DifferentialForm a
               -> [Vector a]
               -> [[Vector a]]
               -> [a]
evalSeparately t omega vs fs = V.toList $ Prelude.foldl S.addV (S.zero l) crossres
  where bvals       = B.tabulateBernstein t vs (fst omegasplit)
        fvals       = [[F.apply ds f eta | f <- fs] | eta <- snd omegasplit]
        crossres    = zipWith (\f v -> V.vector (liftA2 S.mul f v)) fvals bvals
        ds          = P.barycentricGradients t
        omegasplit  = F.split omega
        l           = length vs * length fs
        -- zero        = V.vector (replicate l $ S.embedIntegral 0.0)

inner :: (S.Field a, Ord a) => DifferentialForm a -> DifferentialForm a -> a
inner (F.Form _ _ []) eta   = S.addId
inner omega (F.Form _ _ []) = S.addId
inner omega eta
    | isJust t  = F.inner (B.proj (fromJust t)) omega eta
    | otherwise =
        error "Inner: Need associated simplex to compute inner product."
  where t = findSimplex omega `mplus` findSimplex eta

integrate :: (Show a, S.Field a) => Simplex (Vector a) -> DifferentialForm a -> a
integrate t omega = S.divide (B.integratePolynomial t b) (S.mul kfac vol)
  where b    = apply omega (spanningVectors t)
        -- b'   = S.sclV (S.mulInv (S.mul kfac (volume t))) b
        vol  = if topologicalDimension t == 0 then S.mulId else volume t
        kfac = S.embedIntegral (C.factorial (topologicalDimension t))

trace :: S.Field a => Simplex (Vector a) -> DifferentialForm a -> DifferentialForm a
trace f omega = F.Form (F.arity omega) n' $ F.terms (fmap ((B.redefine f) . (B.trace f)) $ F.trace (sigma f) omega)
  where n' = topologicalDimension f

d :: S.Field a => DifferentialForm a -> DifferentialForm a
d omega = foldr (S.addA . (\i -> fmap (B.deriveBernsteinBasis i) ((F.oneForm i n) S./\ omega)))
          (F.zeroForm (1 + F.arity omega) n)
          [0..n]
  where n = F.dimVec omega

\end{code}
