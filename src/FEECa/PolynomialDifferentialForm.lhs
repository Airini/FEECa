\begin{code}

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings    #-}


module FEECa.PolynomialDifferentialForm where

import Control.Applicative
import Data.Maybe
import Data.List

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
instance Show (BernsteinPolynomial Double) where
  show b = show $ pPrint b
instance  Show (DifferentialForm Double) where
  show omega = show $ printForm ("d" ++ lambda) "0" pPrint (F.terms omega)
-}

findSimplex :: DifferentialForm a -> Maybe (Simplex (Vector a))
findSimplex (F.Form _ _ cs) = find (const True) $ mapMaybe (findSimplex' . fst) cs

findSimplex' :: BernsteinPolynomial a -> Maybe (Simplex (Vector a))
findSimplex' (B.Bernstein t _)  = Just t
findSimplex' _                  = Nothing

tabulate :: (S.Field a, S.VectorSpace a, S.Scalar a ~ a)
         => [DifferentialForm a]
         -> [Vector a]
         -> [Simplex (Vector a)]
         -> [[a]]
tabulate bs vs fs = [evalSeparately t b vs fs' | b <- bs]
  where t   = fromJust $ findSimplex $ head bs
        fs' = map spanningVectors fs

apply :: S.Field a => DifferentialForm a -> [Vector a] -> BernsteinPolynomial a
apply omega vs = {-# SCC "apply" #-} F.apply ds vs omega
  where t  = fromJust (findSimplex omega)
        ds = P.barycentricGradients t

evalSeparately :: (S.Field a, S.VectorSpace a, S.Scalar a ~ a)
               => Simplex (Vector a)
               -> DifferentialForm a
               -> [Vector a]
               -> [[Vector a]]
               -> [a]
evalSeparately t omega vs fs = V.toList $ foldl S.addV zero crossres
  where bvals       = B.tabulateBernstein t vs (fst omegasplit)
        fvals       = [[F.apply ds f eta | f <- fs] | eta <- snd omegasplit]
        crossres    = map V.vector $ zipWith (liftA2 S.mul) fvals bvals
        ds          = P.barycentricGradients t
        omegasplit  = F.split omega
        l           = length vs * length fs
        zero        = V.vector (replicate l $ S.fromDouble 0.0)

inner :: S.Field a => DifferentialForm a -> DifferentialForm a -> a
inner omega eta
    | not (null t) = F.inner (B.proj (head t)) omega eta
    | otherwise =
        error "Inner: Need associated simplex to compute inner product."
  where t = mapMaybe findSimplex [omega, eta]

integrate :: S.Field a => Simplex (Vector a) -> DifferentialForm a -> a
integrate t omega = B.integratePolynomial t b'
  where b'   = S.sclV (S.mulInv (S.mul kfac (volume t))) b
        b    = apply omega (spanningVectors t)
        kfac = S.fromInt (C.factorial (topologicalDimension t))
\end{code}
