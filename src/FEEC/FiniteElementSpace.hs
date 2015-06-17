{-# LANGUAGE FlexibleInstances #-}

module FEEC.FiniteElementSpace (
 -- * Introduction
 -- $intro
)where

import Data.List
import FEEC.Bernstein (BernsteinPolynomial(..), monomial, multiIndices)
import qualified FEEC.Bernstein as B (extend)
import FEEC.Utility.Combinatorics
import FEEC.DifferentialForm
import FEEC.Internal.Form hiding (arity)
import FEEC.Utility.Print
import FEEC.Internal.Simplex
import FEEC.Internal.Spaces
import qualified FEEC.Internal.MultiIndex as MI
import qualified Math.Combinatorics.Exact.Binomial as CBin

-- $intro
-- This file implements the finite element space $P_r\Lambda^k$ and $P_r^-\Lambda^k$
--  in n dimensions. The implementation is based on the article "Geomentric De-
-- compositions and Local Bases for Spaces of Finite Element Differential Forms" by
-- Arnold, Falk, Winther. References given in the comments refer to this article.

-- | Type synomym for basis function of finite element spaces that are of type
-- | Form BernsteinPolynomial.
type BasisFunction = Form BernsteinPolynomial

-- | Data type for the two families of finite element spaces $P_r\Lambda^k$ and
-- | $P_r^-\Lambda^k$ defined over a simplex.
data FiniteElementSpace = PrLk Int Int Simplex
                        | PrmLk Int Int Simplex
                        | GenSpace [BasisFunction]

-- | Name data type to represent the named finite elements. See
-- | <http://www.femtable.org Periodic Table of the Finite Elements>.
data Name = P
          | Pm
          | DP
          | DPm
          | RT
          | N1e
          | N1f
          | BDM
          | N2e
          | N2f

-- | Create named finite element space of given polynomial degree over the
-- | given simplex.
finiteElementSpace :: Name -> Int -> Simplex -> FiniteElementSpace
finiteElementSpace P r t = PrLk r 0 t
finiteElementSpace Pm r t = PrmLk r 0 t
finiteElementSpace DP r t = PrLk r (topologicalDimension t) t
finiteElementSpace DPm r t = PrmLk r (topologicalDimension t) t
finiteElementSpace RT r t
    | (topologicalDimension t) == 2 = PrmLk r 1 t
    | otherwise = error "RT elements are defined in two dimensions."
finiteElementSpace N1e r t
    | (topologicalDimension t) == 3 = PrmLk r 1 t
    | otherwise = error "N1e1 elements are defined in three dimensions."
finiteElementSpace N1f r t
    | (topologicalDimension t) == 3 = PrmLk r 2 t
    | otherwise = error "N1f1 elements are defined in three dimensions."
finiteElementSpace BDM r t
    | (topologicalDimension t) == 2 = PrLk r 1 t
    | otherwise = error "BDM elements are defined in two dimensions."
finiteElementSpace N2e r t
    | (topologicalDimension t) == 3 = PrLk r 1 t
    | otherwise = error "N2e1 elements are defined in three dimensions."
finiteElementSpace N2f r t
    | (topologicalDimension t) == 3 = PrLk r 2 t
    | otherwise = error "N2f1 elements are defined in three dimensions."

-- | The degree of the finite element space.
degree :: FiniteElementSpace -> Int
degree (PrLk r _ _) = r
degree (PrmLk r _ _) = r

-- | The arity of the finite element space
arity :: FiniteElementSpace -> Int
arity (PrLk _ k _) = k
arity (PrmLk _ k _) = k

-- | The dimension of the underlying vector space.
vspaceDim :: FiniteElementSpace -> Int
vspaceDim (PrLk _ _ t) = geometricalDimension t
vspaceDim (PrmLk _ _ t) = geometricalDimension t

-- | The dimension of the finite element space.
instance Dimensioned FiniteElementSpace where
    dim (PrmLk r k t) = ((r + k - 1) `CBin.choose` k) * ((n + k) `CBin.choose` (n - k))
        where n = geometricalDimension t
    dim (PrLk r k t) = ((r + k) `CBin.choose` r ) * ((n + r) `CBin.choose` (n - k))
        where n = geometricalDimension t

-- | List the basis functions of the given finite element space.
basis :: FiniteElementSpace -> [Form BernsteinPolynomial]
basis (PrmLk r k t) = prmLkBasis r k t
basis (PrLk r k t) = prLkBasis r k t

-- | Extend a differential Form defined on a face of a simplex to the full simplex.
-- | Implements the barycentric extension operator defined in eq. (7.1) in
-- | Arnold, Falk, Winther.
extend :: Simplex -> Form BernsteinPolynomial -> Form BernsteinPolynomial
extend t (Form k n' cs) = Form k n (extend' cs)
    where extend' = map (\ (x,y) -> (B.extend t x, extendFace n y))
          n = topologicalDimension t

-- | Extend the face of a subsimplex of a simplex of topological dimension n to the
-- | the simplex. That is, for a face given as an increasing list of vertices of the
-- | subsimplex, return the list of vertices of the simplex that corresponds to that
-- | face.
extendFace :: Int -> [Int] -> [Int]
extendFace n f = [ [0..n] !! (i) | i <- f ]

-- | The range operator for a multi-index and an increasing list.
range :: MI.MultiIndex -> [Int] -> [Int]
range mi sigma = sort (union sigma (MI.range mi))

-- | The basis of the $P_r^-\Lambda^k$ space over the given simplex constructed
-- | using the geometric composition given by Anrold, Falk, Winther.
prmLkBasis :: Int -> Int -> Simplex -> [Form BernsteinPolynomial]
prmLkBasis r k t = concat [ map (extend t) (prmLkFace r k t') | t' <- subsimplices' t k ]

-- | Basis for the space PrMinusLambdak with vanishing trace associated to
-- | a given face of a simplex.1
prmLkFace :: Int -> Int ->Simplex -> [Form BernsteinPolynomial]
prmLkFace r k t = [sclV (b alpha) (whitneyForm t sigma) | alpha <- alphas,
                                                          sigma <- sigmas alpha]
    where n = topologicalDimension t
          b = monomial t
          alphas = MI.degR n (r-1)
          sigmas alpha = [ sigma | sigma <- increasingLists n k,
                                   range alpha sigma == [0..n],
                                   zero alpha sigma ]
          zero alpha sigma = all (0==) (take ((minimum sigma)) (MI.toList alpha))

-- | The Whitney forms as given by  equation (6.3).
whitneyForm :: Simplex -> [Int] -> Form BernsteinPolynomial
whitneyForm t ls = Form k n [( lambda' (ls !! i), subsets !! i) | i <- [0..k]]
    where k = length ls - 1
          n = geometricalDimension t
          subsets = sublists ls
          lambda' i = sclV ((-1)^i) (monomial t (MI.one n i))

-- | Basis of the $P_r\Lambda^k$ space over the given simplex constructed using
-- | the geometric decomposition given by Arnold, Falk, Winther.
prLkBasis :: Int -> Int -> Simplex -> [Form BernsteinPolynomial]
prLkBasis r k t = concat [ map (extend t) (prLkFace r k t') | t' <- subsimplices' t k ]

-- | Basis functions  for the space PrMinusLambdak associated to
-- | a given face of a simplex.
prLkFace :: Int -> Int -> Simplex -> [Form BernsteinPolynomial]
prLkFace r k t = [ sclV (B.extend t b) (psi' (alpha b) sigma) | (b, sigma) <- fs ]
    where fs = map (head . constituents) (prLkFace' r k t)
          psi' alpha sigma = extend t (psi alpha sigma)
          alpha b = (multiIndices b) !! 0
          n = topologicalDimension t

-- | Basis for the space PrMinusLambdak with vanishing trace associated to
-- | a given face of a simplex.
prLkFace' :: Int -> Int -> Simplex -> [Form BernsteinPolynomial]
prLkFace' r k t = [Form k n [((b alpha), sigma)] | alpha <- alphas,
                                                   sigma <- sigmas alpha]
    where n = topologicalDimension t
          b = monomial t
          alphas = MI.degR n r
          sigmas alpha = [ sigma | sigma <- increasingLists n (k-1),
                                   range alpha sigma == [0..n],
                                   zero alpha sigma ]
          zero alpha sigma = all (0==) (take ((minimum' sigma)) (MI.toList alpha))
          minimum' sigma = minimum ([0..n] \\ sigma)

-- | The psi forms that implement the extension operator for the $P_r\Lambda^k$
-- | spaces as given in equations (8.1) and (8.2) in Arnold, Falk, Winther.
psi :: MI.MultiIndex -> [Int] -> Form BernsteinPolynomial
psi alpha sigma  = foldl (/\) unit [psi' alpha i | i <- sigma]
    where unit = nullForm n mulId
          n = dim alpha

-- TODO: Check form indices.
psi' :: MI.MultiIndex -> Int -> Form BernsteinPolynomial
psi' alpha i = subV (db i) (foldl addV zero [sclV (c j) (db j) | j <- [0..n]])
    where db j = oneForm (j+1) n -- Dimension should be n ?
          c j = sclV ((fromIntegral (alpha' !! j)) / (fromIntegral r)) mulId
          zero = zeroForm 1 n
          r = (MI.deg alpha) :: Int
          alpha' = (MI.toList alpha) :: [Int]
          n = dim alpha

t = referenceSimplex 3
t2 = referenceSimplex 2
t1 = subsimplex t 1 3
b = monomial t1 (MI.multiIndex [1,2])
omega = Form 2 2 [(b, [0,1])]

s1 = finiteElementSpace N2f 3 t
