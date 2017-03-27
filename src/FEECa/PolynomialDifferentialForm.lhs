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


type BernsteinPolynomial a = B.BernsteinPolynomial (Vector a) a
type DifferentialForm a    = D.DifferentialForm (BernsteinPolynomial a)

{-
instance  Show (DifferentialForm Double) where
  show omega = show $ printForm ("d" ++ lambda) "0" pPrint (F.terms omega)
-}

findSimplex :: DifferentialForm a -> Maybe (Simplex (Vector a))
findSimplex (F.Form _ _ cs) = getFirst $ foldMap (findSimplex' . fst) cs

findSimplex' :: BernsteinPolynomial a -> First (Simplex (Vector a))
findSimplex' (B.Bernstein t _)  = First $ Just t
findSimplex' _                  = First $ Nothing

tabulate :: (S.Field a, S.VectorSpace a, S.Scalar a ~ a, Ord a)
         => [DifferentialForm a]
         -> [Vector a]
         -> [Simplex (Vector a)]
         -> [[a]]
tabulate bs vs fs = [ evalSeparately t b vs fs' | b <- bs ]
  where t   = fromJust $ findSimplex $ head bs
        fs' = map spanningVectors fs

apply :: (S.Field a, Ord a)
      => DifferentialForm a -> [Vector a] -> BernsteinPolynomial a
apply omega vs = {-# SCC "apply" #-} F.apply ds vs omega
  where t  = fromJust (findSimplex omega)
        ds = P.barycentricGradients t

evalSeparately :: (S.Field a, S.Module a, S.Scalar a ~ a, Ord a)
               => Simplex (Vector a)
               -> DifferentialForm a
               -> [Vector a]
               -> [[Vector a]]
               -> [a]
evalSeparately t omega vs fs = V.toList $ foldl S.addV (S.zero l) crossres
  where bvals       = B.tabulateBernstein t vs (fst omegasplit)
        fvals       = [[F.apply ds f eta | f <- fs] | eta <- snd omegasplit]
        crossres    = zipWith (\f v -> V.vector (liftA2 S.mul f v)) fvals bvals
        ds          = P.barycentricGradients t
        omegasplit  = F.split omega
        l           = length vs * length fs
        -- zero        = V.vector (replicate l $ S.embedIntegral 0.0)

inner :: (S.Field a, Ord a) => DifferentialForm a -> DifferentialForm a -> a
inner omega eta
    | isJust t  = F.inner (B.proj (fromJust t)) omega eta
    | otherwise =
        error "Inner: Need associated simplex to compute inner product."
  where t = findSimplex omega `mplus` findSimplex eta

integrate :: (S.Field a, Ord a) => Simplex (Vector a) -> DifferentialForm a -> a
integrate t omega = S.divide (B.integratePolynomial t b) (S.mul kfac (volume t))
  where b    = apply omega (spanningVectors t)
        b'   = S.sclV (S.mulInv (S.mul kfac (volume t))) b
        kfac = S.embedIntegral (C.factorial (topologicalDimension t))

\end{code}
